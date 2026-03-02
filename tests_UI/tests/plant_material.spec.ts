import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import { page_root, admin_psw, admin_user } from "./utils/constants";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { requestPlantMaterial, wait_request_execution } from "./utils/requests";

const psw: string = "pltmat";
const user = "pltmat_breeder";

test.beforeAll(async ({ browser }) => {
  const context = await browser.newContext();
  const page = await context.newPage();
  await page.goto(page_root);
  await login(page, admin_user, admin_psw);
  await addBreeder(page, user, psw, [
    "no_time_constraint",
    "no_request_size_constraint",
  ]);
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

test("plant_material_request allof", async ({ page }) => {
  test.setTimeout(60000);
  await login(page, user, psw);
  await requestPlantMaterial(page, "pltMat_init_2_allof.txt");
  await wait_request_execution(page, "pltMat_init_2_allof");
});

test("plant_material_request autof", async ({ page }) => {
  test.setTimeout(60000);
  await login(page, user, psw);
  await requestPlantMaterial(page, "pltMat_init_2_autof.txt");
  await wait_request_execution(page, "pltMat_init_2_autof");
});

test("plant_material_request HD", async ({ page }) => {
  test.setTimeout(60000);
  await login(page, user, psw);
  await requestPlantMaterial(page, "pltMat_init_2_HD.txt");
  await wait_request_execution(page, "pltMat_init_2_HD");
});

test("plant_material_request all Methods", async ({ page }) => {
  test.setTimeout(60000);
  await login(page, user, psw);
  await requestPlantMaterial(page, "pltMat_init_allMethods.txt");
  await wait_request_execution(page, "pltMat_init_allMethods");
});
