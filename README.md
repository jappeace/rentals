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
+ house rules.
  + we need to make them accept the house rules during the booking process.
  + some text field fillable by the admin which they've to agree to
+ Discount codes.
+ price rules: 
  + more people booking should increase the price.
  + pets should increase the price
+ variable per night pricing.
  + we mostly try to figure out seasonality but still get full bookings
  + set a minimum price
  + set a maximum price
  + let an "algorightm" decide what to set it to:
     + higher if far in the future
     + lower if closer to now
     + higher if previous bookings around the same time last year

+ reviews with fine grained administration control.

# TODO

+ [x] License
+ [x] Add global namespace
+ [ ] Split persistent models
+ [ ] integrate routes etc


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
