ALTER TABLE "listing" ADD COLUMN "price_per_extra_person" INT8 NOT NULL DEFAULT 0;
ALTER TABLE "listing" ADD COLUMN "max_people" INT8 NOT NULL DEFAULT 1;
