
DROP TABLE IF EXISTS "constants";
CREATE TABLE IF NOT EXISTS "constants" (
	"key"	TEXT NOT NULL PRIMARY KEY,
	"value"	TEXT
);

DROP TABLE IF EXISTS "sessions";
CREATE TABLE IF NOT EXISTS "sessions" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"start"	TEXT,
	"end"	TEXT,
	"year_time"	INTEGER,
	"time_zone"	TEXT
);

DROP TABLE IF EXISTS "breeders";
CREATE TABLE IF NOT EXISTS "breeders" (
	"name"	TEXT NOT NULL PRIMARY KEY,
	"status"	TEXT,
	"h_psw"	TEXT
);

DROP TABLE IF EXISTS "requests";
CREATE TABLE IF NOT EXISTS "requests" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"breeder"	TEXT NOT NULL REFERENCES breeders(name) ON DELETE CASCADE,
	"name"	TEXT,
	"type"	TEXT,
	"game_date"	TEXT,
	"time"	TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"processed" INTEGER DEFAULT 0,
	UNIQUE("breeder", "name")
);


DROP TABLE IF EXISTS "pltmat_requests";
CREATE TABLE IF NOT EXISTS "pltmat_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER NOT NULL REFERENCES requests(id),
	"parent1_id" INTEGER NOT NULL REFERENCES plant_material(id),
	"parent2_id" INTEGER REFERENCES plant_material(id),
	"parent1_request_name" TEXT NOT NULL,
	"parent2_request_name" TEXT,
	"child_name" TEXT NOT NULL,
	"cross_type" TEXT NOT NULL,
	UNIQUE("req_id", "child_name")
);

DROP TABLE IF EXISTS "plant_material";
CREATE TABLE IF NOT EXISTS "plant_material" (
	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
	"name"	TEXT,
	"parent1_id"	INTEGER REFERENCES plant_material(id),
	"parent2_id"	INTEGER REFERENCES plant_material(id),
	"pltmat_request_id"	INTEGER REFERENCES pltmat_requests(id),
	"haplotype_file"	TEXT,
	 UNIQUE("pltmat_request_id", "name")
);

-- CREATE TRIGGER enforce_unique_ind_per_breeder
-- BEFORE INSERT ON plant_material
-- FOR EACH ROW
-- BEGIN
-- 	SELECT CASE
-- 		WHEN (SELECT COUNT(*) FROM v_plant_material
-- 			WHERE (
-- 				(breeder = NEW.breeder OR breeder = '@ALL')
-- 				AND (name = NEW.name)
-- 			) > 0 THEN
-- 			RAISE(ABORT, 'Unique constraint violation on plantmaterial')
-- 	END;
-- END;


CREATE VIEW v_plant_material AS
WITH duration AS (
	SELECT
		MAX(CASE WHEN key = 'duration.allof' THEN value END) AS allof,
		MAX(CASE WHEN key = 'duration.autof' THEN value END) AS autof,
		MAX(CASE WHEN key = 'duration.haplodiplo' THEN value END) AS haplodiplo
	FROM constants
)
SELECT
	pm.id,
	r.breeder as breeder,
	pm.name,
	pm_1.name as parent1_name,
	pm_1.id as parent1_id,
	pm_2.name as parent2_name,
	pm_2.id as parent2_id,
	pr.cross_type,
	CASE
		WHEN pr.cross_type = 'allofecundation' THEN DATE(r.game_date, '+' || d.allof || ' months')
		WHEN pr.cross_type = 'autofecundation' THEN DATE(r.game_date, '+' || d.autof || ' months')
		WHEN pr.cross_type = 'haplodiploidization' THEN DATE(r.game_date, '+' || d.haplodiplo || ' months')
		ELSE NULL
	END AS avail_from,
	pm.haplotype_file,
	pr.id as pltmat_request_id,
	r.id as request_id,
	r.name as request_name,
	r.game_date as request_date,
	r.time as request_time,
	COUNT(pheno.ind_id) as n_pheno,
	COUNT(geno.ind_id) as n_geno
FROM plant_material pm
	INNER JOIN pltmat_requests pr ON (pm.pltmat_request_id = pr.id)
	LEFT JOIN requests r ON (pr.req_id = r.id)
	LEFT JOIN plant_material pm_1 ON (pm.parent1_id = pm_1.id)
	LEFT JOIN plant_material pm_2 ON (pm.parent2_id = pm_2.id)
	JOIN duration d
	LEFT JOIN v_phenotypes pheno ON (pm.id = pheno.ind_id)
	LEFT JOIN v_genotypes geno ON (pm.id = geno.ind_id)
GROUP BY pm.id;


DROP TABLE IF EXISTS "pheno_requests";
CREATE TABLE IF NOT EXISTS "pheno_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER REFERENCES requests(id),
	"ind_id" INTEGER NOT NULL REFERENCES plant_material(id),
	"ind_request_name" TEXT,
	"task" TEXT,
	"n_pheno" INTEGER,
	UNIQUE("req_id", "ind_id", "task")
);


DROP TABLE IF EXISTS "phenotypes";
CREATE TABLE IF NOT EXISTS "phenotypes" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"pheno_req_id" INTEGER INTEGER NOT NULL REFERENCES pheno_requests(id),
	"year" INTEGER NOT NULL,
	"plot" INTEGER NOT NULL,
	"pathogen" INTEGER NOT NULL,
	"trait1" REAL,
	"trait2" REAL,
	"trait3" INTEGER NOT NULL
);

CREATE VIEW v_phenotypes AS
SELECT
	p.id,
	r.breeder,
	pr.ind_id as ind_id,
	pltmat.name as ind,
	p.year,
	p.plot,
	p.pathogen,
	p.trait1,
	p.trait2,
	p.trait3,
	p.pheno_req_id,
	pr.task as type,
	r.name as request_name
FROM phenotypes p
	LEFT JOIN pheno_requests pr ON (p.pheno_req_id = pr.id)
	LEFT JOIN plant_material pltmat ON (pr.ind_id = pltmat.id)
	LEFT JOIN requests r ON (pr.req_id = r.id);



DROP TABLE IF EXISTS "geno_requests";
CREATE TABLE IF NOT EXISTS "geno_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER REFERENCES requests(id),
	"ind_id" INTEGER NOT NULL REFERENCES plant_material(id),
	"ind_request_name" TEXT,
	"type" TEXT,
	UNIQUE("req_id", "ind_id", "type")
);

DROP TABLE IF EXISTS "genotypes";
CREATE TABLE IF NOT EXISTS "genotypes" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"geno_req_id" INTEGER NOT NULL REFERENCES geno_requests(id),
	"type" TEXT TEXT NOT NULL,
	"result_file" TEXT NOT NULL,
	UNIQUE("geno_req_id", "type")
);

CREATE VIEW v_genotypes AS
SELECT
	g.id,
	r.breeder,
	gr.ind_id as ind_id,
	pltmat.name as ind,
	g.type,
	g.result_file,
	g.geno_req_id,
	r.name as request_name
FROM genotypes g
	LEFT JOIN geno_requests gr ON (g.geno_req_id = gr.id)
	LEFT JOIN plant_material pltmat ON (gr.ind_id = pltmat.id)
	LEFT JOIN requests r ON (gr.req_id = r.id);

