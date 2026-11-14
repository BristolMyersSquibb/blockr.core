// Setting the page's tracing up as the page starts, as Tracing has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing):
// wherever the blockr option otel_client_endpoint names a collector, the page
// records spans and sends them to its path v1/traces, whatever the blockr
// option logger says, as R records spans wherever OpenTelemetry exports
// traces. Nowhere else does the page record a span, nor a request carry a
// traceparent.
// Core sets the tracing up through its OpenTelemetry module, which the page
// imports for it only where it sends records (../page/page.ts), the module
// that sends its log records too (../log/settings.ts).

import { collectorUrl, log } from "../log/index.ts";
import type { OtelModule } from "../log/index.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// The URL that the page sends its spans to, where the blockr option
// otel_client_endpoint names a collector, with a relative URL read against
// `base`, the page's, or undefined where it names none. It throws for a value
// that is no URL over HTTP or HTTPS.
export function tracesUrl(otelClientEndpoint: Json | undefined, base?: string): string | undefined {
  return otelClientEndpoint === undefined ? undefined : collectorUrl(otelClientEndpoint, "v1/traces", base);
}

// Sets the page's tracing up where `url` names where its spans go, once `load`
// has imported core's OpenTelemetry module, which names the session of each
// span by `sessionId`. It resolves once the module has set the tracing up, or
// once it failed to, which core logs, and at once where `url` is undefined.
export async function setUpTracing(
  url: string | undefined,
  sessionId: () => string | undefined,
  load: () => Promise<OtelModule>,
): Promise<void> {
  if (url === undefined) return;
  try {
    (await load()).otelTracing({ url, sessionId });
  } catch (error) {
    log.error(
      new Error(`Core could not set up its OpenTelemetry tracing, so the page records no spans: ${errorMessage(error)}`, {
        cause: error,
      }),
    );
  }
}
