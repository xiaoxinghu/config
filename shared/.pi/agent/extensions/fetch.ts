/**
 * Fetch Tool Extension
 *
 * Registers a `fetch` tool the LLM can call to retrieve content from a URL.
 *
 * By default it routes web pages through readable.page's /md endpoint
 * (https://readable.page/md?url=...), which extracts the main content and
 * returns clean Markdown — stripping ads, nav, and boilerplate to save tokens.
 *
 * Use raw=true to bypass readable.page and fetch the URL directly (JSON APIs,
 * raw files, non-GET requests). If readable.page can't extract a page, the tool
 * automatically falls back to a direct fetch.
 *
 * Uses Node's built-in fetch (no dependencies).
 * Install: this file lives in ~/.pi/agent/extensions/ so it loads globally.
 * Reload in a running session with /reload.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { StringEnum } from "@earendil-works/pi-ai";
import { Type } from "typebox";

const READABLE_MD_ENDPOINT = "https://readable.page/md";
const DEFAULT_MAX_LENGTH = 50_000;
const DEFAULT_TIMEOUT_MS = 30_000;

/** Strip HTML down to readable-ish plain text (fallback only). */
function htmlToText(html: string): string {
	return html
		.replace(/<script\b[^>]*>[\s\S]*?<\/script>/gi, "")
		.replace(/<style\b[^>]*>[\s\S]*?<\/style>/gi, "")
		.replace(/<noscript\b[^>]*>[\s\S]*?<\/noscript>/gi, "")
		.replace(/<\/(p|div|section|article|header|footer|li|tr|h[1-6]|blockquote)>/gi, "\n")
		.replace(/<br\s*\/?>/gi, "\n")
		.replace(/<\/?(ul|ol)\b[^>]*>/gi, "\n")
		.replace(/<[^>]+>/g, "")
		.replace(/&nbsp;/gi, " ")
		.replace(/&amp;/gi, "&")
		.replace(/&lt;/gi, "<")
		.replace(/&gt;/gi, ">")
		.replace(/&quot;/gi, '"')
		.replace(/&#39;/gi, "'")
		.replace(/[ \t]+/g, " ")
		.replace(/\n[ \t]+/g, "\n")
		.replace(/\n{3,}/g, "\n\n")
		.trim();
}

export default function fetchExtension(pi: ExtensionAPI) {
	pi.registerTool({
		name: "fetch",
		label: "Fetch",
		description:
			"Fetch a URL. By default web pages are routed through readable.page to extract " +
			"the main content and return clean Markdown (no ads/nav/boilerplate, fewer tokens). " +
			"Set raw=true to fetch the URL directly and return the unmodified body — use this for " +
			"JSON APIs, raw files, or non-GET requests. If readable.page can't extract a page, the " +
			"tool falls back to a direct fetch automatically.",
		promptSnippet: "Fetch a URL as clean Markdown (raw=true for direct/raw fetch)",
		promptGuidelines: [
			"Use fetch to read a web page or article as Markdown; pass raw=true for JSON APIs, raw files, or non-GET requests.",
		],
		parameters: Type.Object({
			url: Type.String({ description: "The URL to fetch (must be http or https)" }),
			raw: Type.Optional(
				Type.Boolean({
					description:
						"Bypass readable.page and fetch the URL directly, returning the unmodified body. " +
						"Use for JSON APIs, raw files, or non-GET requests (default false).",
				}),
			),
			method: Type.Optional(
				StringEnum(["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD"] as const, {
					description: "HTTP method for raw mode (default GET; ignored in markdown mode)",
				}),
			),
			headers: Type.Optional(
				Type.Record(Type.String(), Type.String(), {
					description: "Optional request headers for raw mode (ignored in markdown mode)",
				}),
			),
			body: Type.Optional(
				Type.String({ description: "Optional request body for raw mode (POST/PUT/PATCH)" }),
			),
			maxLength: Type.Optional(
				Type.Number({
					description: `Max characters of body to return before truncation (default ${DEFAULT_MAX_LENGTH})`,
				}),
			),
			timeoutMs: Type.Optional(
				Type.Number({
					description: `Request timeout in milliseconds (default ${DEFAULT_TIMEOUT_MS})`,
				}),
			),
		}),

		async execute(_toolCallId, params, signal, onUpdate, _ctx) {
			let target: URL;
			try {
				target = new URL(params.url);
			} catch {
				throw new Error(`Invalid URL: ${params.url}`);
			}
			if (target.protocol !== "http:" && target.protocol !== "https:") {
				throw new Error(`Unsupported protocol: ${target.protocol} (only http/https allowed)`);
			}

			const maxLength = params.maxLength ?? DEFAULT_MAX_LENGTH;
			const timeoutMs = params.timeoutMs ?? DEFAULT_TIMEOUT_MS;

			// Combine the agent abort signal with a timeout.
			const newSignal = () => {
				const timeout = AbortSignal.timeout(timeoutMs);
				return {
					timeout,
					composite: signal ? AbortSignal.any([signal, timeout]) : timeout,
				};
			};

			const doFetch = async (
				url: URL,
				init: RequestInit,
			): Promise<{ response: Response; cancelled: boolean }> => {
				const { timeout, composite } = newSignal();
				try {
					const response = await fetch(url, { ...init, redirect: "follow", signal: composite });
					return { response, cancelled: false };
				} catch (err) {
					if (timeout.aborted) throw new Error(`Request timed out after ${timeoutMs}ms: ${url.href}`);
					if (signal?.aborted) return { response: undefined as never, cancelled: true };
					throw new Error(`Fetch failed: ${(err as Error).message}`);
				}
			};

			const finish = (
				headerLines: (string | null)[],
				body: string,
				details: Record<string, unknown>,
			) => {
				let out = body;
				let truncated = false;
				if (out.length > maxLength) {
					out = out.slice(0, maxLength);
					truncated = true;
				}
				const header = [...headerLines, truncated ? `Body truncated to ${maxLength} characters` : null]
					.filter(Boolean)
					.join("\n");
				const text = out ? `${header}\n\n${out}` : header;
				return { content: [{ type: "text", text }], details: { ...details, truncated } };
			};

			// ---- Raw mode: direct fetch, unmodified body ----------------------
			if (params.raw) {
				const method = params.method ?? "GET";
				onUpdate?.({ content: [{ type: "text", text: `${method} ${target.href} (raw)` }] });
				const { response, cancelled } = await doFetch(target, {
					method,
					headers: params.headers,
					body: params.body,
				});
				if (cancelled) return { content: [{ type: "text", text: "Cancelled" }], details: {} };

				const contentType = response.headers.get("content-type") ?? "";
				const rawBody = method === "HEAD" ? "" : await response.text();
				return finish(
					[
						`${method} ${target.href} (raw)`,
						`Status: ${response.status} ${response.statusText}`,
						`Content-Type: ${contentType || "(none)"}`,
						response.url !== target.href ? `Final URL: ${response.url}` : null,
					],
					rawBody,
					{
						mode: "raw",
						url: target.href,
						finalUrl: response.url,
						status: response.status,
						contentType,
						bytes: rawBody.length,
					},
				);
			}

			// ---- Markdown mode: route through readable.page /md ---------------
			onUpdate?.({ content: [{ type: "text", text: `Fetching markdown for ${target.href}` }] });
			const readableUrl = new URL(READABLE_MD_ENDPOINT);
			readableUrl.searchParams.set("url", target.href);

			const { response: mdResponse, cancelled } = await doFetch(readableUrl, { method: "GET" });
			if (cancelled) return { content: [{ type: "text", text: "Cancelled" }], details: {} };

			const mdContentType = mdResponse.headers.get("content-type") ?? "";
			if (mdResponse.ok && /text\/markdown/i.test(mdContentType)) {
				const markdown = await mdResponse.text();
				return finish(
					[`Source: ${target.href}`, "Format: Markdown (via readable.page)"],
					markdown,
					{
						mode: "markdown",
						url: target.href,
						status: mdResponse.status,
						bytes: markdown.length,
					},
				);
			}

			// readable.page couldn't extract -> fall back to a direct fetch.
			onUpdate?.({
				content: [{ type: "text", text: `readable.page extraction failed; fetching ${target.href} directly` }],
			});
			const { response: fb, cancelled: fbCancelled } = await doFetch(target, { method: "GET" });
			if (fbCancelled) return { content: [{ type: "text", text: "Cancelled" }], details: {} };

			const fbContentType = fb.headers.get("content-type") ?? "";
			const fbBody = await fb.text();
			const isHtml = /text\/html|application\/xhtml\+xml/i.test(fbContentType);
			const converted = isHtml && fbBody.length > 0;
			const body = converted ? htmlToText(fbBody) : fbBody;

			return finish(
				[
					`Source: ${target.href}`,
					"Format: direct fetch (readable.page extraction failed)",
					`Status: ${fb.status} ${fb.statusText}`,
					`Content-Type: ${fbContentType || "(none)"}`,
					converted ? "Body: HTML converted to text" : null,
				],
				body,
				{
					mode: "fallback",
					url: target.href,
					finalUrl: fb.url,
					status: fb.status,
					contentType: fbContentType,
					converted,
					bytes: fbBody.length,
				},
			);
		},
	});
}
