import { test, expect, Page } from "@playwright/test";
import { readFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import gunzip from "gunzip-file";

const psw: string = "1234";
const page_root: string = "http://127.0.0.1:3000";
interface Registerd_indsList {
  [key: string]: string[];
}

test.describe("PlantBreedGame_UI", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(page_root);
  });

  test("game_initialisation", async ({ page }) => {
    test.setTimeout(200_000);
    await page.goto(page_root);
    await page.getByRole("link", { name: "gears icon Admin" }).click();
    await page.getByRole("button", { name: "Initialise Game" }).click();
    await expect(
      page.getByRole("heading", {
        name: "Apimeta simulans , a species with a bright future!",
      }),
    ).toBeVisible({
      timeout: 200_000, // game initialisation can be quite long
    });
  });

  test("basicLogin", async ({ page }) => {
    await login(page, "admin", psw);
  });

  test("add and delete breeder", async ({ page }) => {
    await login(page, "admin", psw);
    await addBreeder(page, "toto", psw, "tester");
    await deleteBreeder(page, "toto");
  });

  test("addBreeder", async ({ page }) => {
    await login(page, "admin", psw);
    await addBreeder(page, "test_UI", psw, "tester");
    await login(page, "test_UI", psw);
    await initialData(page);
  });

  test("phenotyping_request", async ({ page }) => {
    await login(page, "test_UI", psw);
    await requestPhenotyping(page, "pheno_init_2_allMethods.txt");
  });

  test("plantMaterial_request", async ({ page }) => {
    await login(page, "test_UI", psw);
    await requestPlantMaterial(page, "pltMat_init_set1.txt");
  });

  // Some problem can occure because the SNP ids are generated
  // randomly during the "game set-up" therefore the specified snp id in the
  // test files may not be available in the list of snp that can be genotyped.
  // this should work with the default `plantbreedgame_setup.Rmd`
  test("Genotyping_request", async ({ page }) => {
    await login(page, "test_UI", psw);
    await requestGenotyping(page, "geno_init_3_allMethods.txt");
  });

  test("register individual", async ({ page }) => {
    test.setTimeout(60_000);
    await login(page, "admin", psw);
    await registerIndividuals(page, [
      "Coll0101",
      "Coll0102",
      "Coll0103",
      "Coll0104",
      "Coll0105",
    ]);
  });

  // TODO add tests where several requests are made with the same file
  // this is a edge case that could happend

  test("evaluation", async ({ page }) => {
    test.setTimeout(120_000);

    var registered_inds: Registerd_indsList = {
      admin: ["Coll0101", "Coll0102", "Coll0103", "Coll0104", "Coll0105"],
      // test_UI: ["G2_001", "G2_002", "G2_003", "G2_004", "G2_005"],
      test_UI: ["Coll0201", "Coll0202", "Coll0203", "Coll0204", "Coll0205"],
    };

    // register individuals:
    for (let breeder in registered_inds) {
      await page.goto(page_root);
      await login(page, breeder, psw);
      await registerIndividuals(page, registered_inds[breeder], false);
    }

    await runEvaluation(page, registered_inds);
  });

  // TODO: add a test for "Admin / GameProgress" the core function is partially checked
  // with the evaluation check (when we build the game report) but a

  test("delete breeder", async ({ page }) => {
    await login(page, "admin", psw);
    await deleteBreeder(page, "test_UI");
  });

  test("Large_requests/full campaing", async ({ page }) => {
    test.setTimeout(180_000);
    await login(page, "admin", psw);
    await addBreeder(page, "test_UI", psw, "tester");

    await login(page, "admin", psw);
    await addBreeder(page, "test_UI_2", psw, "tester");

    await login(page, "test_UI", psw);

    await requestPhenotyping(page, "pheno_init_100_field.txt");
    await requestPhenotyping(page, "pheno_init_100_pahto.txt");

    await requestGenotyping(page, "geno_init_100_hd.txt");
    await requestGenotyping(page, "geno_init_100_ld.txt");
    await requestGenotyping(page, "geno_init_100_snp.txt");

    await requestPlantMaterial(page, "pltMat_init_set100.txt", 60_000);
    // });

    // test.only("2nd_generation", async ({ page }) => {
    await requestPhenotyping(page, "pheno_G1_set100.txt");
    await requestGenotyping(page, "geno_G1_set100.txt");
    await requestPlantMaterial(page, "pltMat_G1_set100.txt");

    var registered_inds: Registerd_indsList = {
      // admin: ["Coll0101", "Coll0102", "Coll0103", "Coll0104", "Coll0105"],
      test_UI: ["G2_001", "G2_002", "G2_003", "G2_004", "G2_005"],
      test_UI_2: ["Coll0101", "Coll0102", "Coll0103", "Coll0104", "Coll0105"],
    };

    // register individuals:
    for (let breeder in registered_inds) {
      await page.goto(page_root);
      await login(page, breeder, psw);
      await registerIndividuals(page, registered_inds[breeder], false);
    }

    await runEvaluation(page, registered_inds);
  });

  test("game re-initialisation", async ({ page }) => {
    test.setTimeout(200_000);
    await login(page, "admin", psw);
    await page.getByRole("link", { name: "gears icon Admin" }).click();
    await page.getByRole("link", { name: "Game Initialisation" }).click();
    await page.getByLabel("Confirmation:").fill("plantbreedgame");
    await page.getByRole("button", { name: "Initialise Game" }).click();
    await expect(
      page.getByRole("heading", {
        name: "Apimeta simulans , a species with a bright future!",
      }),
    ).toBeVisible({
      timeout: 200_000, // game initialisation can be quite long
    });

    // simple game actions
    await login(page, "admin", psw);
    await addBreeder(page, "test_UI", psw, "tester");
    await login(page, "test_UI", psw);

    await requestPhenotyping(page, "pheno_init_2_allMethods.txt");
    await requestPlantMaterial(page, "pltMat_init_set1.txt");
    await requestGenotyping(page, "geno_init_3_allMethods.txt");

    var registered_inds: Registerd_indsList = {
      test_UI: ["Coll0101", "Coll0102", "Coll0103", "Coll0104", "Coll0105"],
    };
    for (let breeder in registered_inds) {
      await registerIndividuals(page, registered_inds[breeder], false);
    }
    await runEvaluation(page, registered_inds);
  });
});

async function login(page: Page, username: string, password: string) {
  await page.goto(page_root);
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();
  await page
    .locator("div")
    .filter({ hasText: /^admin$/ })
    .nth(1)
    .click();
  await expect(
    page.getByRole("option", { name: username, exact: true }),
  ).toBeVisible();
  await page.getByRole("option", { name: username, exact: true }).click();
  await page.getByLabel("Password").click();
  await page.getByLabel("Password").fill(password);
  await page.getByRole("button", { name: "Log in" }).click();

  // top bar visible ?
  await expect(page.locator("#breederBoxID")).toBeVisible();
  await expect(page.getByRole("heading", { name: username })).toBeVisible();
  await expect(page.locator("#dateBoxID")).toBeVisible();
  await expect(page.locator("#budgetBoxID")).toBeVisible();
  await expect(page.locator("#serverIndicID")).toBeVisible();

  // content
  await expect(
    page.getByRole("heading", { name: "Phenotyping data:" }),
  ).toBeVisible();
  await expect(
    page.getByRole("heading", { name: "Genotyping data:" }),
  ).toBeVisible();
  await expect(
    page.getByRole("heading", { name: "Plant material data:" }),
  ).toBeVisible();
  await expect(page.getByRole("heading", { name: "Other:" })).toBeVisible();
}

async function addBreeder(
  page: Page,
  breederName: string,
  password: string,
  status: string,
) {
  // Add new breeder
  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(
    page.getByRole("link", { name: "Manage breeders" }),
  ).toBeVisible();
  await page.getByRole("link", { name: "Manage breeders" }).click();
  await expect(page.locator("#newBreederName")).toBeVisible();
  await page.locator("#newBreederName").click();
  await page.locator("#newBreederName").fill(breederName);
  await page.getByText("player").nth(1).click();
  await page.getByRole("option", { name: status }).click();
  await page.getByLabel("Password", { exact: true }).click();
  await page.getByLabel("Password", { exact: true }).fill(password);
  await page.getByRole("button", { name: "Add this new breeder" }).click();

  await expect(page.getByText("Adding breeder Done!")).toBeVisible();

  // TODO: check we cannot add already existing breeders (low priority)
}

async function deleteBreeder(page: Page, breederName: string) {
  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(
    page.getByRole("link", { name: "Manage breeders" }),
  ).toBeVisible();
  await page.getByRole("link", { name: "Manage breeders" }).click();
  await page.locator("#delBreederName-selectized").click();
  await page.getByRole("option", { name: breederName, exact: true }).click();
  await page
    .getByRole("button", {
      name: "DO NOT click! (unless you are sure to delete this breeder)",
    })
    .click();
  await expect(page.getByText("Deleting breeder Done!")).toBeVisible({});
}

async function requestPlantMaterial(
  page: Page,
  reqFile: string,
  req_timeout: number = 5000,
) {
  // plant material small request
  await page
    .getByRole("link", { name: "seedling icon Request plant material" })
    .click();
  await page
    .getByRole("textbox", { name: "Browse..." })
    .setInputFiles("./tests/requestExamples/" + reqFile);

  await page.getByRole("link", { name: "Check" }).click();
  await expect(page.locator("#plmatUploaded")).toContainText("GOOD");

  await page.getByRole("link", { name: "Summary" }).click();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page.getByRole("link", { name: "Data" }).click();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("parent1: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("parent2: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("child: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("explanations: activate to sort column ascending"),
  ).toBeVisible();

  await page.getByRole("link", { name: "Request", exact: true }).click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();
  await expect(page.getByText("Create Plant Material: Done !")).toBeVisible({
    timeout: req_timeout,
  });
  await page.getByText("×").click();

  // TODO: need to find better way to check we can download the
  // plant material file
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();
  // await page.locator("#pltMatFile-selectized").click();
  // await page.getByRole("option", { name: "IndList_2024-12-31.txt" }).click();

  await requestFileCopyAvailable(page, reqFile, "pltMat");

  // let reqFileNoExt = reqFile.replace(/\.[^.]+$/, "");
  await expect(async () => {
    await page.locator("#pltMatFile-selectized").click();
    let pat = `IndList_\\d{4}-\\d{2}-\\d{2}`;
    await expect(
      page.getByRole("option").getByText(new RegExp(pat)).first(),
    ).toBeVisible({ timeout: 100 });
  }).toPass();
}

async function requestPhenotyping(
  page: Page,
  reqFile: string,
  req_timeout: number = 5000,
) {
  await page
    .getByRole("link", { name: "flask icon Request phenotyping" })
    .click();

  await page
    .getByRole("textbox", { name: "Browse..." })
    .setInputFiles("./tests/requestExamples/" + reqFile);

  await page.getByRole("link", { name: "Check" }).click();
  await expect(page.locator("#PhenoUploaded")).toContainText("GOOD");

  await page.getByRole("link", { name: "Summary" }).click();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page.getByRole("link", { name: "Data" }).click();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("ind: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("task: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("details: activate to sort column ascending"),
  ).toBeVisible();

  await page.getByRole("link", { name: "Request", exact: true }).click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();
  await expect(page.getByText("Process Pheno request: Done")).toBeVisible({
    timeout: req_timeout,
  });
  await page.getByText("×").click();

  // check request is done
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();

  await requestFileCopyAvailable(page, reqFile, "pheno");
  await checkResultsFile(page, reqFile, "pheno");
}

async function requestGenotyping(
  page: Page,
  reqFile: string,
  req_timeout: number = 5000,
) {
  await page.getByRole("link", { name: "dna icon Request genotyping" }).click();
  await page
    .getByRole("textbox", { name: "Browse..." })
    .setInputFiles("./tests/requestExamples/" + reqFile);

  await page.getByRole("link", { name: "Check" }).click();
  await expect(page.locator("#GenoUploaded")).toContainText("GOOD");

  await page.getByRole("link", { name: "Summary" }).click();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page.getByRole("link", { name: "Data" }).click();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("ind: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("task: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("details: activate to sort column ascending"),
  ).toBeVisible();

  await page.getByRole("link", { name: "Request", exact: true }).click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();
  await expect(page.getByText("Process Geno request: Done")).toBeVisible({
    timeout: req_timeout,
  });
  await page.getByText("×").click();

  // check request is done
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();

  await requestFileCopyAvailable(page, reqFile, "geno");

  await checkResultsFile(page, reqFile, "geno");
  // // request results file is available
  // const reqFileNoExt = reqFile.replace(/\.[^.]+$/, "");
  // var resultFilePatern = new RegExp(
  //   `Result_genos-((hd)|(ld)|(single-snps))_${reqFileNoExt}_\\d{4}-\\d{2}-\\d{2}`,
  // );

  // await expect(async () => {
  //   await expect(page.locator("#genoFile-selectized")).toBeVisible({
  //     timeout: 100,
  //   });
  //   await page.locator("#genoFile-selectized").locator("..").click();
  //   await expect(
  //     page.getByRole("option").getByText(resultFilePatern).first(),
  //   ).toBeVisible({ timeout: 100 });
  // }).toPass({ timeout: 5000 });
  // await page.getByRole("option").getByText(resultFilePatern).first().click();

  // // download request results file
  // const [download] = await Promise.all([
  //   page.waitForEvent("download"),
  //   await page.locator("#dwnlGeno").click(),
  // ]);
  // const tempFilePath = join(tmpdir(), `${Date.now()}_genoResults.txt`);
  // const downloadPath = await download.path();
  // gunzip(downloadPath, tempFilePath, function () {
  //   const fileContent = readFileSync(tempFilePath, { encoding: "utf8" });
  //   expect(fileContent).toContain("snp");
  // });
}

async function checkResultsFile(page: Page, reqFile: string, type: string) {
  let fileSelectorID: string = "";
  let buttonID: string = "";
  let expectedText: string = "";
  let tmpFileSuff: string = "";
  var resultFilePatern = new RegExp("");
  const reqFileNoExt = reqFile.replace(/\.[^.]+$/, "");

  if (type === "pheno") {
    fileSelectorID = "#phenoFile-selectized";
    buttonID = "#dwnlPheno";
    tmpFileSuff = "phenoResults.txt";
    resultFilePatern = new RegExp(
      `Result_pheno-((field)|(patho))_${reqFileNoExt}_\\d{4}-\\d{2}-\\d{2}.txt.gz`,
    );
    expectedText = "ind\tyear\tplot\tpathogen\ttrait1\ttrait2\ttrait3";
  }

  if (type === "geno") {
    fileSelectorID = "#genoFile-selectized";
    buttonID = "#dwnlGeno";
    tmpFileSuff = "genoResults.txt";
    resultFilePatern = new RegExp(
      `Result_genos-((hd)|(ld)|(single-snps))_${reqFileNoExt}_\\d{4}-\\d{2}-\\d{2}`,
    );
    expectedText = "snp";
  }

  await expect(async () => {
    await expect(page.locator(fileSelectorID)).toBeVisible({
      timeout: 100,
    });
    await page.locator(fileSelectorID).locator("..").click();
    await expect(
      page.getByRole("option").getByText(resultFilePatern).first(),
    ).toBeVisible({ timeout: 100 });
  }).toPass({ timeout: 5000 });
  await page.getByRole("option").getByText(resultFilePatern).first().click();

  // download request results file
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    await page.locator(buttonID).click(),
  ]);
  const tempFilePath = join(tmpdir(), `${Date.now()}_${tmpFileSuff}.txt`);
  const downloadPath = await download.path();
  gunzip(downloadPath, tempFilePath, function () {
    const fileContent = readFileSync(tempFilePath, { encoding: "utf8" });
    expect(fileContent).toContain(expectedText);
  });
}

async function initialData(page: Page) {
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();

  // pheno files
  await expect(page.locator("#phenoFile-selectized")).toBeVisible();
  await page.locator("#phenoFile-selectized").click();
  await expect(page.getByRole("option")).toHaveCount(2);
  await expect(
    page.getByRole("option", { name: "Result_phenos_controls.txt.gz" }),
  ).toBeVisible();
  await expect(
    page.getByRole("option", {
      name: "Result_phenos_initialColl.txt.gz",
    }),
  ).toBeVisible();

  // geno files
  await expect(page.locator("#genoFile-selectized")).toBeVisible();
  await page.locator("#genoFile-selectized").click();
  await expect(page.getByRole("option")).toHaveCount(1);
  await expect(
    page.getByRole("option", {
      name: "Result_genos_subset-initialColl-hd",
    }),
  ).toBeVisible();

  // other files
  await expect(page.locator("#requestFile-selectized")).toBeVisible();
  await page.locator("#requestFile-selectized").click();
  await expect(page.getByRole("option")).toHaveCount(5);
  await expect(
    page.getByRole("option", {
      name: "controls.txt",
    }),
  ).toBeVisible();
  await expect(
    page.getByRole("option", {
      name: "example_request_data.txt",
    }),
  ).toBeVisible();
  await expect(
    page.getByRole("option", {
      name: "example_request_plant_material.txt",
    }),
  ).toBeVisible();
  await expect(
    page.getByRole("option", {
      name: "snp_coords_hd.txt.gz",
    }),
  ).toBeVisible();
  await expect(
    page.getByRole("option", {
      name: "snp_coords_ld.txt.gz",
    }),
  ).toBeVisible();
}

async function requestFileCopyAvailable(
  page: Page,
  reqFile: string,
  reqType: string,
) {
  await expect(async () => {
    await page.locator("#requestFile-selectized").click();
    await expect(
      page.getByRole("option", {
        name: "Request-" + reqType + "_" + reqFile,
      }),
    ).toBeVisible({ timeout: 200 });
  }).toPass({ timeout: 5000 });
  await page
    .getByRole("option", { name: "Request-" + reqType + "_" + reqFile })
    .click();
}

async function registerIndividuals(
  page: Page,
  add_individuals: string[] = [
    "Coll0001",
    "Coll0002",
    "Coll0003",
    "Coll0004",
    "Coll0005",
  ],
  full_check: boolean = true,
) {
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();

  await expect(
    page.getByRole("link", { name: "Register final individuals" }),
  ).toBeVisible();
  await page.getByRole("link", { name: "Register final individuals" }).click();
  await expect(
    page.locator(".tab-pane .active").locator(".selectize-input"),
  ).toBeVisible();

  if (full_check) {
    // add and delete a single individual
    var new_ind = "Coll0042";
    await page.locator(".tab-pane .active").locator(".selectize-input").click();
    await page.keyboard.type(new_ind);
    await page.keyboard.press("Enter");
    await page.keyboard.press("Escape");
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
    await page.getByRole("button", { name: "Submit" }).click();
    await expect(page.getByRole("cell", { name: new_ind })).toBeVisible();

    await expect(page.getByRole("cell", { name: new_ind })).toBeVisible();
    await page.getByRole("cell", { name: new_ind }).click();
    await expect(
      page.getByRole("cell", { name: new_ind }).locator(".."),
    ).toHaveClass(/active/);
    await page.getByRole("button", { name: "Delete" }).click();
    await expect(page.getByRole("cell", { name: new_ind })).toHaveCount(0);

    // no more than 5 ind can be added
    var ind_list = ["Coll0001", "Coll0002", "Coll0003", "Coll0004", "Coll0005"];
    await page.locator(".tab-pane .active").locator(".selectize-input").click();
    for (let ind of ind_list) {
      await page.keyboard.type(ind);
      await page.keyboard.press("Enter");
    }
    await page.keyboard.press("Escape");
    await page.getByRole("button", { name: "Submit" }).click();

    var new_ind = "Coll0042";
    await page.locator(".tab-pane .active").locator(".selectize-input").click();
    await page.keyboard.type(new_ind);
    await page.keyboard.press("Enter");
    await page.keyboard.press("Escape");
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
    await page.getByRole("button", { name: "Submit" }).click();
    await expect(page.getByRole("cell", { name: new_ind })).toHaveCount(0);

    await page.locator(".tab-pane .active").locator(".selectize-input").click();
    await page.keyboard.press("Backspace");

    for (let ind of ind_list) {
      await page.getByRole("cell", { name: ind }).click();
    }
    await page.getByRole("button", { name: "Delete" }).click();
  }

  // add specified inds
  await page.locator(".tab-pane .active").locator(".selectize-input").click();
  for (let ind of add_individuals) {
    await page.keyboard.type(ind);
    await page.keyboard.press("Enter");
  }
  await page.keyboard.press("Escape");
  await page.getByRole("button", { name: "Submit" }).click();

  for (let ind of add_individuals) {
    await expect(page.getByRole("cell", { name: ind })).toBeVisible();
  }

  // TODO: Check the registered individuals apprears in the "evaluation" page.
  // But this do not seems straight forward to as the table showing the registered
  // individuals is splitted in several pages.
  // This may be better checked by running the evaluation and check the registered
  // individuals appears in the results.
}

async function runEvaluation(page: Page, registered_inds: Registerd_indsList) {
  await expect(
    page.getByRole("link", { name: "medal icon Evaluation" }),
  ).toBeVisible();
  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await page.getByRole("button", { name: "Launch evaluation!" }).click();
  await expect(page.locator("#evalGraphT1")).toBeVisible();

  // check T1
  await expect(page.locator("#evalGraphT1")).toBeVisible();
  await expect(
    page.locator("#evalGraphT1").filter({
      // "control*Coll0163control*Coll0389control*Coll0598control*Coll0611control*Coll0758",
      hasText: "control*",
    }),
  ).toBeVisible();

  for (let breeder in registered_inds) {
    for (let ind of registered_inds[breeder]) {
      await expect(
        page.locator("#evalGraphT1").filter({
          hasText: breeder + "*" + ind,
        }),
      ).toBeVisible();
    }
  }

  // check T2
  await page.getByRole("link", { name: "Trait 2" }).click();
  await expect(page.locator("#evalGraphT2")).toBeVisible();
  await expect(
    page.locator("#evalGraphT2").filter({
      hasText: "control*",
    }),
  ).toBeVisible();

  for (let breeder in registered_inds) {
    for (let ind of registered_inds[breeder]) {
      await expect(
        page.locator("#evalGraphT2").filter({
          hasText: breeder + "*" + ind,
        }),
      ).toBeVisible();
    }
  }

  // check T3
  await page.getByRole("link", { name: "Trait 3" }).click();
  await expect(page.locator("#evalGraphT3")).toBeVisible();
  await expect(
    page.locator("#evalGraphT3").filter({
      hasText: "control*",
    }),
  ).toBeVisible();

  for (let breeder in registered_inds) {
    for (let ind of registered_inds[breeder]) {
      await expect(
        page.locator("#evalGraphT3").filter({
          hasText: breeder + "*" + ind,
        }),
      ).toBeVisible();
    }
  }

  // check T1xT2
  await page.getByRole("link", { name: "Traits 1 vs 2" }).click();
  await expect(page.locator("#evalGraphT1vT2")).toBeVisible();
  for (let graph_elements of Object.keys(registered_inds).concat([
    "control",
    "Linear regression",
    "Initial Collection",
  ])) {
    await expect(
      page.locator("#evalGraphT1vT2").filter({
        hasText: graph_elements,
      }),
    ).toBeVisible();
  }

  // check pedigree
  await page.getByRole("link", { name: "Pedigree" }).click();
  await expect(page.locator("#evalUIpedigree")).toBeVisible();
  await expect(page.locator("#evalPlotPedigree")).toBeVisible();

  await expect(page.locator("#evalUIpedigree .selectize-input")).toBeVisible();
  for (let breeder in registered_inds) {
    await page.locator("#evalUIpedigree .selectize-input").click();
    await expect(
      page
        .locator("#evalUIpedigree .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIpedigree .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    await expect(page.getByRole("img", { name: "Plot object" })).toBeVisible();
  }

  // check allele frequencies
  await page.getByRole("link", { name: "AFs" }).click();
  await expect(page.locator("#evalUIAfsPlot")).toBeVisible();

  await expect(page.locator("#propAFS")).toBeVisible();
  await expect(page.locator("#evalUIAfsPlot .selectize-input")).toBeVisible();
  for (let breeder in registered_inds) {
    await page.locator("#evalUIAfsPlot .selectize-input").click();
    await expect(
      page
        .locator("#evalUIAfsPlot .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIAfsPlot .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();

    await expect(
      page.locator("#evalGraphAFsHist .plotly").filter({
        hasText: "initial AFs",
      }),
    ).toBeVisible({ timeout: 10000 });
    await expect(
      page.locator("#evalGraphAFsScatter .plotly").filter({
        hasText: "initial allele frequencies",
      }),
    ).toBeVisible({ timeout: 10000 });
  }

  // check additive relationships
  await page.getByRole("link", { name: "Additive relationships" }).click();
  await expect(page.locator("#evalUIaddRelation")).toBeVisible();
  await expect(page.locator("#addRelTable")).toBeVisible();

  await expect(
    page.locator("#evalUIaddRelation .selectize-input"),
  ).toBeVisible();
  for (let breeder in registered_inds) {
    await page.locator("#evalUIaddRelation .selectize-input").click();
    await expect(
      page
        .locator("#evalUIaddRelation .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIaddRelation .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    await expect(page.locator("#addRelTable")).toBeVisible();
  }

  // check request history
  await page.getByRole("link", { name: "Requests history" }).click();
  await expect(page.locator("#evalUIrequestHistory")).toBeVisible();

  await expect(
    page.locator("#evalUIrequestHistory .selectize-input"),
  ).toBeVisible();

  for (let breeder of Object.keys(registered_inds).concat([
    "--- All Breeders ---",
  ])) {
    await page.locator("#evalUIrequestHistory .selectize-input").click();
    await expect(
      page
        .locator("#evalUIrequestHistory .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIrequestHistory .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    await expect(page.locator("#historyTable")).toBeVisible();
  }

  // check players' scores
  await page.getByRole("link", { name: "Players' scores" }).click();
  await expect(page.locator("#evalUIgameScores")).toBeVisible();
  await expect(
    page.locator("#evalUIgameScores .selectize-input"),
  ).toBeVisible();

  for (let score of ["T1 with sufficient quality", "Product T1 x T2"]) {
    await page.locator("#evalUIgameScores .selectize-input").click();
    await expect(
      page
        .locator("#evalUIgameScores .selectize-dropdown")
        .getByRole("option", { name: score }),
    ).toBeVisible();
    await page
      .locator("#evalUIgameScores .selectize-dropdown")
      .getByRole("option", { name: score })
      .click();

    await expect(
      page.getByRole("button", { name: "medal icon Find the Winner !" }),
    ).toBeVisible();
    await expect(page.locator("#averageScore")).toBeVisible();
    await expect(page.locator("#T3_penalty-label")).toBeVisible();

    if (score === "T1 with sufficient quality") {
      await expect(page.locator("#evalUI_t2penalty")).toBeVisible();
    }

    page.getByRole("button", { name: "medal icon Find the Winner !" }).click();
    await expect(page.locator("#podium")).toBeVisible();
    await expect(page.locator("#scoreTable")).toBeVisible();
    await expect(page.locator("#scoreTable thead")).toHaveText(/breeder/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/ind/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/score/);

    for (let breeder in registered_inds) {
      await expect(
        page.locator("#podium ol").getByText(breeder, { exact: true }),
      ).toBeVisible();

      for (let ind of registered_inds[breeder]) {
        await expect(
          page.getByRole("cell", { name: breeder + "*" + ind }),
        ).toBeVisible();
      }
    }

    await page.getByLabel("Average accross all submited individuals").click();
    page.getByRole("button", { name: "medal icon Find the Winner !" }).click();
    await expect(page.locator("#scoreTable")).toBeVisible();

    await expect(
      page.locator("#scoreTable thead").getByText(/ind/),
    ).toHaveCount(0);
    await expect(page.locator("#scoreTable thead")).toHaveText(/breeder/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/score/);
    for (let breeder in registered_inds) {
      await expect(
        page
          .locator("#scoreTable")
          .getByRole("cell", { name: breeder, exact: true }),
      ).toBeVisible();
    }
    await page.getByLabel("Average accross all submited individuals").click();
  }

  //check download report
  await page.getByRole("link", { name: "Download Report" }).click();
  await expect(page.locator("#elvalReport")).toBeVisible();
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    await page.locator("#elvalReport").click(),
  ]);

  expect(readFileSync(await download.path(), { encoding: "utf8" })).toContain(
    "Breeding Game, Final Results",
  );
}
