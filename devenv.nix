{ pkgs, ... }:

{
  env.SDL_RENDER_DRIVER = "software";

  packages = [pkgs.SDL2];

  languages.haskell.enable = true;
}
