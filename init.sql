-- Initialize the rental database with user, permissions, and test data.
--
-- Usage:
--   1. Run as a PostgreSQL superuser to create user and database:
--        psql -f init.sql
--   2. Start the app once so it runs migrations:
--        cabal run rentals
--   3. Seed the test data (skip the first section since it's already done):
--        psql -h localhost -U rental -d rental -f init.sql
--      (the CREATE/GRANT statements will fail harmlessly on the second run)

-- =============================================================================
-- Database setup (run as superuser)
-- =============================================================================

CREATE USER rental WITH PASSWORD 'rental';
CREATE DATABASE rental;
GRANT ALL ON DATABASE rental TO rental;
\c rental
GRANT USAGE, CREATE ON SCHEMA public TO rental;

-- =============================================================================
-- Test data (run after the app has booted once to create the schema)
-- =============================================================================

-- Listings
INSERT INTO "listing" ("title", "description", "price", "cleaning", "country", "address", "handler_name", "handler_phone", "slug", "uuid", "currency")
VALUES
  ('Seaside Cottage', 'A cozy cottage with ocean views, sleeps 4. Private beach access and fully equipped kitchen.', 12000, 5000, 'Netherlands', 'Strandweg 42, 2586 JK Den Haag', 'Jan de Vries', '+31 6 12345678', 'seaside-cottage', 'a1b2c3d4-e5f6-7890-abcd-ef1234567890', 'EUR'),
  ('Mountain Cabin', 'Rustic cabin in the Alps with stunning views. Perfect for skiing in winter, hiking in summer.', 15000, 7500, 'Austria', 'Bergstraße 7, 6020 Innsbruck', 'Hans Müller', '+43 664 9876543', 'mountain-cabin', 'b2c3d4e5-f6a7-8901-bcde-f12345678901', 'EUR'),
  ('City Apartment', 'Modern apartment in the heart of Amsterdam. Walking distance to museums and restaurants.', 9500, 3000, 'Netherlands', 'Herengracht 100, 1015 BS Amsterdam', 'Pieter Jansen', '+31 6 98765432', 'city-apartment', 'c3d4e5f6-a7b8-9012-cdef-123456789012', 'EUR');

-- Events (bookings and blocked dates)
-- source must be a valid Source value: Local, Airbnb, or Vrbo
INSERT INTO "event" ("listing", "source", "uuid", "start", "end", "price", "description", "summary", "blocked", "booked")
VALUES
  (1, 'Local', 'evt-001-aaa', '2026-04-10', '2026-04-15', 12000, 'Spring getaway', 'Booking: Smith family', false, true),
  (1, 'Local', 'evt-002-bbb', '2026-04-20', '2026-04-22', 12000, NULL, 'Owner blocked', true, false),
  (2, 'Airbnb', 'evt-003-ccc', '2026-05-01', '2026-05-08', 15000, 'Ski holiday', 'Booking: Müller party', false, true),
  (3, 'Vrbo', 'evt-004-ddd', '2026-04-12', '2026-04-14', 9500, 'Weekend trip', 'Booking: Weekend visitor', false, true);

-- Checkouts
INSERT INTO "checkout" ("listing", "event", "session_id", "name", "email", "emailed")
VALUES
  (1, 1, 'sess-abc-123', 'John Smith', 'john.smith@example.com', true),
  (2, 3, 'sess-def-456', 'Anna Müller', 'anna.mueller@example.com', true),
  (3, 4, 'sess-ghi-789', 'Bob Johnson', 'bob.johnson@example.com', false);

-- Imports (external calendar syncs)
-- source must be a valid Source value: Local, Airbnb, or Vrbo
INSERT INTO "import" ("listing", "source", "uri")
VALUES
  (1, 'Airbnb', 'https://www.airbnb.com/calendar/ical/12345.ics?s=example'),
  (2, 'Vrbo', 'https://www.vrbo.com/icalendar/example-cabin.ics');

-- Listing images (uuid must be valid UUID format)
INSERT INTO "listing_image" ("listing", "uuid")
VALUES
  (1, '11111111-1111-1111-1111-111111111101'),
  (1, '11111111-1111-1111-1111-111111111102'),
  (2, '22222222-2222-2222-2222-222222222201'),
  (3, '33333333-3333-3333-3333-333333333301'),
  (3, '33333333-3333-3333-3333-333333333302'),
  (3, '33333333-3333-3333-3333-333333333303');
