begin;

ALTER TABLE "listing" DROP COLUMN "currency";

delete from schema_migrations where filename = '20241020_193352-add-currency.sql';

commit;
