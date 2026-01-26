import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import {
  add_registerIndividual,
  goto_ind_registration,
} from "./utils/inds_registration";
import { page_root, admin_psw, admin_user } from "./utils/constants";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import {
  requestGenotyping,
  requestPhenotyping,
  requestPlantMaterial,
  wait_request_execution,
} from "./utils/requests";

const psw: string = "campaing";
const user = "campaing";

const inds_to_register = {
  campaing: ["G2_001", "G2_002", "G2_003", "G2_004", "G2_005"],
};

test.beforeAll(async ({ browser }) => {
  const context = await browser.newContext();
  const page = await context.newPage();
  await page.goto(page_root);
  await login(page, admin_user, admin_psw);
  await addBreeder(page, user, psw, "tester");
  await context.close();
});

test.afterAll(async ({ browser }) => {
  const context = await browser.newContext();
  const page = await context.newPage();
  await page.goto(page_root);
  await login(page, admin_user, admin_psw);
  await deleteBreeder(page, user);
  await context.close();
});

test.beforeEach(async ({ page }) => {
  await page.goto(page_root);
});

test("Large_requests/full campaing", async ({ page }) => {
  test.setTimeout(180_000);

  await login(page, user, psw);
  await requestPhenotyping(page, "pheno_init_100_field.txt");
  await requestPhenotyping(page, "pheno_init_100_pahto.txt");

  await requestGenotyping(page, "geno_init_100_hd.txt");
  await requestGenotyping(page, "geno_init_100_ld.txt");
  await requestGenotyping(page, "geno_init_100_snp.txt");

  await requestPlantMaterial(page, "pltMat_init_set100.txt");

  await wait_request_execution(page, "pheno_init_100_field");
  await wait_request_execution(page, "pheno_init_100_pahto");
  await wait_request_execution(page, "geno_init_100_hd");
  await wait_request_execution(page, "geno_init_100_ld");
  await wait_request_execution(page, "geno_init_100_snp");
  await wait_request_execution(page, "pltMat_init_set100");

  // 2nd_generation
  await requestPhenotyping(page, "pheno_G1_set100.txt");
  await requestGenotyping(page, "geno_G1_set100.txt");
  await requestPlantMaterial(page, "pltMat_G1_set100.txt");

  await wait_request_execution(page, "pheno_G1_set100");
  await wait_request_execution(page, "geno_G1_set100");
  await wait_request_execution(page, "pltMat_G1_set100");

  await login(page, user, user); // needed to be sure the individuals
  // registration input is updated with the newlly created individuals
  // TODO: this should be improved on the app
  await goto_ind_registration(page);
  const new_inds = inds_to_register[user];
  await add_registerIndividual(page, new_inds);
  for (let ind of new_inds) {
    await expect(page.getByRole("cell", { name: ind })).toBeVisible();
  }
});
