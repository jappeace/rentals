-- Initialize the rental database, user, and test data.
-- Run as a PostgreSQL superuser:
--   psql -f init.sql

-- Create user and database
CREATE USER rental WITH PASSWORD 'rental';
CREATE DATABASE rental;
GRANT ALL ON DATABASE rental TO rental;

-- Connect to the new database and set up permissions
\c rental
GRANT USAGE, CREATE ON SCHEMA public TO rental;

-- Apply schema (same as migrations/up)
CREATE TABLE "listing"(
  "id" SERIAL8 PRIMARY KEY UNIQUE,
  "title" VARCHAR NOT NULL,
  "description" VARCHAR NOT NULL,
  "price" INT8 NOT NULL,
  "cleaning" INT8 NOT NULL DEFAULT 0,
  "country" VARCHAR NOT NULL DEFAULT '',
  "address" VARCHAR NOT NULL DEFAULT '',
  "handler_name" VARCHAR NOT NULL DEFAULT '',
  "handler_phone" VARCHAR NOT NULL DEFAULT '',
  "slug" VARCHAR NOT NULL,
  "uuid" VARCHAR NOT NULL,
  "currency" VARCHAR NOT NULL DEFAULT 'USD'
);
ALTER TABLE "listing" ADD CONSTRAINT "unique_slug" UNIQUE("slug");
ALTER TABLE "listing" ADD CONSTRAINT "unique_calendar" UNIQUE("uuid");

CREATE TABLE "event"(
  "id" SERIAL8 PRIMARY KEY UNIQUE,
  "listing" INT8 NOT NULL,
  "source" VARCHAR NOT NULL,
  "uuid" VARCHAR NOT NULL,
  "start" DATE NOT NULL,
  "end" DATE NOT NULL,
  "price" INT8 NULL,
  "description" VARCHAR NULL,
  "summary" VARCHAR NULL,
  "blocked" BOOLEAN NOT NULL DEFAULT false,
  "booked" BOOLEAN NOT NULL DEFAULT false
);
ALTER TABLE "event" ADD CONSTRAINT "unique_event" UNIQUE("listing","start");
ALTER TABLE "event" ADD CONSTRAINT "event_listing_fkey" FOREIGN KEY("listing") REFERENCES "listing"("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

CREATE TABLE "checkout"(
  "id" SERIAL8 PRIMARY KEY UNIQUE,
  "listing" INT8 NOT NULL,
  "event" INT8 NOT NULL,
  "session_id" VARCHAR NOT NULL,
  "name" VARCHAR NOT NULL,
  "email" VARCHAR NOT NULL,
  "emailed" BOOLEAN NOT NULL
);
ALTER TABLE "checkout" ADD CONSTRAINT "unique_checkout" UNIQUE("session_id");
ALTER TABLE "checkout" ADD CONSTRAINT "unique_checkout_event" UNIQUE("event");
ALTER TABLE "checkout" ADD CONSTRAINT "checkout_listing_fkey" FOREIGN KEY("listing") REFERENCES "listing"("id") ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE "checkout" ADD CONSTRAINT "checkout_event_fkey" FOREIGN KEY("event") REFERENCES "event"("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

CREATE TABLE "import"(
  "id" SERIAL8 PRIMARY KEY UNIQUE,
  "listing" INT8 NOT NULL,
  "source" VARCHAR NOT NULL,
  "uri" VARCHAR NOT NULL
);
ALTER TABLE "import" ADD CONSTRAINT "unique_import" UNIQUE("listing","source");
ALTER TABLE "import" ADD CONSTRAINT "import_listing_fkey" FOREIGN KEY("listing") REFERENCES "listing"("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

CREATE TABLE "listing_image"(
  "id" SERIAL8 PRIMARY KEY UNIQUE,
  "listing" INT8 NOT NULL,
  "uuid" VARCHAR NOT NULL
);
ALTER TABLE "listing_image" ADD CONSTRAINT "unique_image" UNIQUE("uuid");
ALTER TABLE "listing_image" ADD CONSTRAINT "listing_image_listing_fkey" FOREIGN KEY("listing") REFERENCES "listing"("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

-- Schema migration tracking (used by postgresql-migration)
CREATE TABLE IF NOT EXISTS "schema_migrations"(
  "filename" VARCHAR NOT NULL PRIMARY KEY,
  "checksum" VARCHAR NOT NULL,
  "executed_at" TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Grant table permissions to the rental user
GRANT ALL ON ALL TABLES IN SCHEMA public TO rental;
GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO rental;

-- =============================================================================
-- Test data
-- =============================================================================

-- Listings
INSERT INTO "listing" ("title", "description", "price", "cleaning", "country", "address", "handler_name", "handler_phone", "slug", "uuid", "currency")
VALUES
  ('Seaside Cottage', 'A cozy cottage with ocean views, sleeps 4. Private beach access and fully equipped kitchen.', 12000, 5000, 'Netherlands', 'Strandweg 42, 2586 JK Den Haag', 'Jan de Vries', '+31 6 12345678', 'seaside-cottage', 'a1b2c3d4-e5f6-7890-abcd-ef1234567890', 'EUR'),
  ('Mountain Cabin', 'Rustic cabin in the Alps with stunning views. Perfect for skiing in winter, hiking in summer.', 15000, 7500, 'Austria', 'Bergstraße 7, 6020 Innsbruck', 'Hans Müller', '+43 664 9876543', 'mountain-cabin', 'b2c3d4e5-f6a7-8901-bcde-f12345678901', 'EUR'),
  ('City Apartment', 'Modern apartment in the heart of Amsterdam. Walking distance to museums and restaurants.', 9500, 3000, 'Netherlands', 'Herengracht 100, 1015 BS Amsterdam', 'Pieter Jansen', '+31 6 98765432', 'city-apartment', 'c3d4e5f6-a7b8-9012-cdef-123456789012', 'EUR');

-- Events (bookings and blocked dates)
INSERT INTO "event" ("listing", "source", "uuid", "start", "end", "price", "description", "summary", "blocked", "booked")
VALUES
  (1, 'manual', 'evt-001-aaa', '2026-04-10', '2026-04-15', 12000, 'Spring getaway', 'Booking: Smith family', false, true),
  (1, 'manual', 'evt-002-bbb', '2026-04-20', '2026-04-22', 12000, NULL, 'Owner blocked', true, false),
  (2, 'manual', 'evt-003-ccc', '2026-05-01', '2026-05-08', 15000, 'Ski holiday', 'Booking: Müller party', false, true),
  (3, 'manual', 'evt-004-ddd', '2026-04-12', '2026-04-14', 9500, 'Weekend trip', 'Booking: Weekend visitor', false, true);

-- Checkouts
INSERT INTO "checkout" ("listing", "event", "session_id", "name", "email", "emailed")
VALUES
  (1, 1, 'sess-abc-123', 'John Smith', 'john.smith@example.com', true),
  (2, 3, 'sess-def-456', 'Anna Müller', 'anna.mueller@example.com', true),
  (3, 4, 'sess-ghi-789', 'Bob Johnson', 'bob.johnson@example.com', false);

-- Imports (external calendar syncs)
INSERT INTO "import" ("listing", "source", "uri")
VALUES
  (1, 'airbnb', 'https://www.airbnb.com/calendar/ical/12345.ics?s=example'),
  (2, 'vrbo', 'https://www.vrbo.com/icalendar/example-cabin.ics');

-- Listing images
INSERT INTO "listing_image" ("listing", "uuid")
VALUES
  (1, 'img-seaside-001'),
  (1, 'img-seaside-002'),
  (2, 'img-mountain-001'),
  (3, 'img-city-001'),
  (3, 'img-city-002'),
  (3, 'img-city-003');
