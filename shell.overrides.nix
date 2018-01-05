self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = self: super: {
      ghcWithPackages = super.ghcWithHoogle;
    };
  };
}
