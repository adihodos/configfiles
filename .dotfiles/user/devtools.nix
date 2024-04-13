{ config, pkgs, setupOptions, ... }:
{
  home.packages = with pkgs; [
    # rustup
    # # gcc
    # clang
    # clang-tools
    # lldb
    # gdb
    # cmake
    # autoconf
    # automake
    # libtool
  ];
}
