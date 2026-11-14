import type { OtelModule } from "../log/index.ts";
import type { Json } from "../types/json.ts";
export declare function tracesUrl(otelClientEndpoint: Json | undefined, base?: string): string | undefined;
export declare function setUpTracing(url: string | undefined, sessionId: () => string | undefined, load: () => Promise<OtelModule>): Promise<void>;
