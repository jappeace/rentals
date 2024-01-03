Rentals.

A self hosted airbnb like rental system.

The major advantage is that
it's a fixed hosting costs instead of payment per booking,
which would be around $50 per month if done by a third party,
or if done by yourself whatever price you can rent the hardware.
Which may range from 2 to 10 dollars.

Features
---

- Homepage with all listings
- Custom pics and description per listing.
- Book a rental trough stripe.
- Custom pricing
- Syncs with airbnb/vrbo for blocked dates.

# Usage

install devenv,
install nix

```
cabal run
```

the app binds by default to port 3023.


you can enter the shell manually with:

```
nix develop
```

or build without nix using plain cabal,
however that will miss several overrides.
