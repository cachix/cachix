{ ghcVersion ? "902" }: (import ./.).outputs.stack-shell.${builtins.currentSystem} {inherit ghcVersion;}
