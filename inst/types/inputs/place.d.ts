export interface Box {
    readonly left: number;
    readonly top: number;
    readonly bottom: number;
    readonly width: number;
}
export interface Size {
    readonly width: number;
    readonly height: number;
}
export interface Spot {
    readonly left: number;
    readonly top: number;
}
export declare const MIN_WIDTH = 190;
export declare function listWidth(field: Box): number;
export declare function listSpot(field: Box, list: Size, view: Size): Spot;
