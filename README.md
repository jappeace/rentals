Rentals.

A self hosted airbnb like rental system.

The major advantage is that
it's a fixed hosting costs instead of payment per booking,
which would be around $50 per month if done by a third party,
or if done by yourself whatever price you can rent the hardware.

Features
---

- [] homepage
  - [] show all listings, paginated
    - [] open each listing individually
    - [] search listings
  - [] login options
- [] listing
  - [] show available dates for booking
  - [] show info like title, description, price
- [] admin
  - [] add new listings
  - [] import calendars for a given listing
  - [] export calendars for a given listing

# Planned features
+ Discount codes.
+ variable pricing.
+ reviews with fine grained administration control.

# TODO

+ [ ] License
+ [ ] Add global namespace
+ [ ] Split persistent models
+ [ ] integrate routes etc


# Usage

install devenv,
install nix

```
cabal run
```

the app binds by default to port 3023.

