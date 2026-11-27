// The build preset that core ships to packages, as `@blockr/core/build`,
// which extends the preset of blockr.ui with core's own entry points.
// A package imports the vendor check and staticImports(), which lists what
// each file of a build imports up front, from `@blockr/ui/build`, whichever
// preset it spreads
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).

export { preset } from "./preset.ts";
