
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
	"req_id" INTEGER NOT NULL REFERENCES requests(id) ON DELETE CASCADE,
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
	"pltmat_request_id"	INTEGER REFERENCES pltmat_requests(id) ON DELETE CASCADE,
	"haplotype_file"	TEXT,
	"control"	INTEGER DEFAULT 0,
	"GV_trait1"	FLOAT,
	"GV_trait2"	FLOAT,
	"resist_trait3"	INTEGER,
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
		ELSE DATE(NULL)
	END AS avail_from,
	pm.haplotype_file,
	pm.control,
	CASE
		WHEN ers.ind_id IS NOT NULL AND ers.selected == 1 THEN 1
		ELSE 0
	END as selected_for_evaluation,
	pm.GV_trait1,
	pm.GV_trait2,
	pm.resist_trait3,
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
	LEFT JOIN v_evaluation_requests_summary ers ON (ers.ind_id = pm.id)
GROUP BY pm.id;


DROP TABLE IF EXISTS "pheno_requests";
CREATE TABLE IF NOT EXISTS "pheno_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER REFERENCES requests(id)  ON DELETE CASCADE,
	"ind_id" INTEGER NOT NULL REFERENCES plant_material(id),
	"ind_request_name" TEXT,
	"type" TEXT,
	"n_pheno" INTEGER,
	UNIQUE("req_id", "ind_id", "type")
);


DROP TABLE IF EXISTS "phenotypes";
CREATE TABLE IF NOT EXISTS "phenotypes" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"pheno_req_id" INTEGER INTEGER NOT NULL REFERENCES pheno_requests(id) ON DELETE CASCADE,
	"year" INTEGER NOT NULL,
	"plot" TEXT NOT NULL,
	"pathogen" INTEGER NOT NULL,
	"trait1" REAL,
	"trait2" REAL,
	"trait3" INTEGER NOT NULL
);


DROP TABLE IF EXISTS "evaluation_requests";
CREATE TABLE IF NOT EXISTS "evaluation_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER NOT NULL REFERENCES requests(id) ON DELETE CASCADE,
	"action" TEXT NOT NULL,
	"ind_id" INTEGER NOT NULL REFERENCES plant_material(id)
);

CREATE VIEW v_evaluation_requests_summary AS
SELECT
	ind_id,
	SUM(CASE WHEN action = 'add' THEN 1 ELSE -1 END) as selected
FROM evaluation_requests
GROUP BY ind_id;


CREATE VIEW v_phenotypes AS
WITH duration AS (
	SELECT
		MAX(CASE WHEN key = 'max.upload.pheno.field' THEN value END) AS start_day_pheno_field,
		MAX(CASE WHEN key = 'duration.pheno.field' THEN value END) AS duration_pheno_field,
		MAX(CASE WHEN key = 'duration.pheno.patho' THEN value END) AS duration_pheno_patho
	FROM constants
)
SELECT
	p.id,
	r.breeder,
	pr.ind_id as ind_id,
	pltmat.name as ind,
	pltmat.control as control_ind,
	p.year,
	p.plot,
	p.pathogen,
	p.trait1,
	p.trait2,
	p.trait3,
	p.pheno_req_id,
	pr.type as type,
	r.name as request_name,
	r.game_date as request_game_date,
	CASE
		WHEN pr.type = 'pheno-patho' THEN DATE(r.game_date, '+' || d.duration_pheno_patho || ' months')
		WHEN pr.type = 'pheno-field' THEN DATE(p.year || '-' || d.start_day_pheno_field, '+' || d.duration_pheno_field || ' months')
		ELSE DATE(NULL)
	END AS avail_from
FROM phenotypes p
	LEFT JOIN pheno_requests pr ON (p.pheno_req_id = pr.id)
	LEFT JOIN plant_material pltmat ON (pr.ind_id = pltmat.id)
	JOIN duration d
	LEFT JOIN requests r ON (pr.req_id = r.id);



DROP TABLE IF EXISTS "geno_requests";
CREATE TABLE IF NOT EXISTS "geno_requests" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"req_id" INTEGER REFERENCES requests(id) ON DELETE CASCADE,
	"ind_id" INTEGER NOT NULL REFERENCES plant_material(id),
	"ind_request_name" TEXT,
	"type" TEXT,
	UNIQUE("req_id", "ind_id", "type")
);

DROP TABLE IF EXISTS "genotypes";
CREATE TABLE IF NOT EXISTS "genotypes" (
	"id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"geno_req_id" INTEGER NOT NULL REFERENCES geno_requests(id) ON DELETE CASCADE,
	"type" TEXT TEXT NOT NULL,
	"result_file" TEXT NOT NULL,
	UNIQUE("geno_req_id", "type")
);

CREATE VIEW v_genotypes AS
WITH duration AS (
	SELECT
		MAX(CASE WHEN key = 'duration.geno.hd' THEN value END) AS duration_geno_hd,
		MAX(CASE WHEN key = 'duration.geno.ld' THEN value END) AS duration_geno_ld,
		MAX(CASE WHEN key = 'duration.geno.single' THEN value END) AS duration_geno_snp
	FROM constants
)
SELECT
	g.id,
	r.breeder,
	gr.ind_id as ind_id,
	pltmat.name as ind,
	g.type,
	g.result_file,
	g.geno_req_id,
	r.name as request_name,
	CASE
		WHEN gr.type = 'hd' THEN DATE(r.game_date, '+' || d.duration_geno_hd || ' months')
		WHEN gr.type = 'ld' THEN DATE(r.game_date, '+' || d.duration_geno_ld || ' months')
		ELSE DATE(r.game_date, '+' || d.duration_geno_snp || ' months')
	END AS avail_from
FROM genotypes g
	LEFT JOIN geno_requests gr ON (g.geno_req_id = gr.id)
	LEFT JOIN plant_material pltmat ON (gr.ind_id = pltmat.id)
	LEFT JOIN requests r ON (gr.req_id = r.id)
	JOIN duration d;

CREATE VIEW v_request_history AS
WITH prices AS (
	SELECT
		CASE
			WHEN key = "cost.pheno.field" THEN "pheno-field"
			WHEN key = "cost.pheno.patho" THEN "pheno-patho"
			WHEN key = "cost.allof" THEN "allofecundation"
			WHEN key = "cost.autof" THEN "autofecundation"
			WHEN key = "cost.haplodiplo" THEN "haplodiploidization"
			WHEN key = "cost.geno.hd" THEN "hd"
			WHEN key = "cost.geno.ld" THEN "ld"
			WHEN key = "cost.geno.single" THEN "geno-snp"
			WHEN key = "cost.register" THEN "evaluation submission"
			ELSE NULL
		END AS detail,
		CASE
			WHEN key = "cost.pheno.field" THEN CAST(value AS FLOAT)
			ELSE CAST(value AS FLOAT) * (
				SELECT
					CAST(value AS FLOAT)
				FROM
					constants
				WHERE
					key = "cost.pheno.field"
			)
		END as unit_price
	FROM
		constants
	WHERE
		detail is not NULL
), durations AS (
	SELECT
		CASE
			WHEN key = "duration.pheno.field" THEN "pheno-field"
			WHEN key = "duration.pheno.patho" THEN "pheno-patho"
			WHEN key = "duration.allof" THEN "allofecundation"
			WHEN key = "duration.autof" THEN "autofecundation"
			WHEN key = "duration.haplodiplo" THEN "haplodiploidization"
			WHEN key = "duration.geno.hd" THEN "hd"
			WHEN key = "duration.geno.ld" THEN "ld"
			WHEN key = "duration.geno.single" THEN "geno-snp"
			ELSE NULL
		END AS detail,
		value as duration
	FROM constants
	WHERE
		detail is not NULL
), request_details AS (
	SELECT
		r.id,
		r.game_date,
		r.time,
		r.breeder,
		r.name,
		r.type as request_type,
		CASE
			WHEN r.type = 'pltmat' THEN pltr.cross_type
			WHEN r.type = 'pheno' THEN pr."type"
			WHEN r.type = 'geno' THEN
				CASE
					WHEN gr."type" LIKE "snp%" THEN "geno-snp"
					ELSE gr.type
				END
			WHEN r.type LIKE 'evaluation%' THEN r.type
			ELSE NULL
		END AS detail,
		CASE
				WHEN r.type = 'pltmat' THEN COUNT(*)
				WHEN r.type = 'pheno' THEN SUM(pr.n_pheno)
				WHEN r.type = 'geno' THEN COUNT(*)
				WHEN r.type LIKE 'evaluation%' THEN COUNT(*)
				ELSE NULL
		END AS quantity,
		r.processed
	FROM requests r
	  FULL JOIN pheno_requests pr ON (pr.req_id = r.id)
	  FULL JOIN pltmat_requests pltr ON (pltr.req_id = r.id)
	  FULL JOIN geno_requests gr ON (gr.req_id = r.id)
	  FULL JOIN evaluation_requests er ON (er.req_id = r.id)
	GROUP BY r.id, detail
) SELECT
	d.*,
	p.unit_price,
	d.quantity * p.unit_price as cost,
	CASE
		WHEN d.detail == "pheno-field" THEN CASE
			WHEN d.game_date < DATE(STRFTIME('%Y', d.game_date) || '-' || c.value)
			THEN DATE(STRFTIME('%Y', d.game_date) || '-' || c.value, "+" || t.duration || " months")
			ELSE DATE((CAST(STRFTIME('%Y', d.game_date) AS INTEGER) + 1) || '-' || c.value, "+" || t.duration || " months")
		END
		ELSE DATE(d.game_date, '+' || t.duration || ' months')
	END as avail_from
FROM request_details d
LEFT JOIN prices p ON d.detail = p.detail
LEFT JOIN durations t ON d.detail = t.detail
CROSS JOIN (
	SELECT value
	FROM constants
	WHERE key = 'max.upload.pheno.field'
) c
ORDER BY d.time;



CREATE VIEW v_remaining_budget AS
SELECT
	breeder,
	total_spent,
	(SELECT value FROM constants WHERE key = 'initialBudget') - total_spent as remaining_budget
FROM (
	SELECT
		b.name as breeder,
		coalesce(sum(rh.cost), 0) as total_spent
	FROM breeders b
	FULL JOIN v_request_history rh ON (b.name = rh.breeder)
	GROUP BY b.name
)

