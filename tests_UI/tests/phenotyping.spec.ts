import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import { page_root, admin_psw, admin_user } from "./utils/constants";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { requestPhenotyping, wait_request_execution } from "./utils/requests";
import { readFileSync } from "fs";

const psw: string = "pheno";
const user = "pheno_breeder";

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

test("phenotyping_request", async ({ page }) => {
  await login(page, user, psw);
  await requestPhenotyping(page, "pheno_init_2_allMethods.txt");
  await wait_request_execution(page, "pheno_init_2_allMethods");
});

test("download phenotype top button", async ({ page }) => {
  await login(page, user, psw);
  await download_phenotype(page, "pheno_init_2_allMethods", "1");
});

test("download phenotype bottom button", async ({ page }) => {
  await login(page, user, psw);
  await download_phenotype(page, "pheno_init_2_allMethods", "2");
});

async function download_phenotype(
  page: Page,
  request_name: string,
  button: string, // 2 buttons are availables "1" or "2"
) {
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();
  await page.getByRole("link", { name: "Phenotype data" }).click();

  await page
    .locator("#pheno_download_pheno_filter-pheno_requests-selectized")
    .click();
  await page.getByRole("option", { name: request_name }).click();
  await page.keyboard.press("Escape");

  const button_id = "#dwnlPheno_" + button;

  await expect(page.locator(button_id)).toBeVisible();
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    await page.locator(button_id).click(),
  ]);

  const downloadPath = await download.path();
  expect(downloadPath).toBeTruthy();
  expect(readFileSync(await downloadPath, { encoding: "utf8" })).toContain(
    "trait1",
  );
}
