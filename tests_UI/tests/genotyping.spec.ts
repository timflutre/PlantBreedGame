import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import { page_root, admin_psw, admin_user } from "./utils/constants";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { requestGenotyping, wait_request_execution } from "./utils/requests";

const psw: string = "geno";
const user = "geno_breeder";

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

test("genotyping_request", async ({ page }) => {
  await login(page, user, psw);
  await requestGenotyping(page, "geno_init_3_allMethods.txt");
  await wait_request_execution(page, "geno_init_3_allMethods");
});

test("download_genotypes hd", async ({ page }) => {
  await login(page, user, psw);
  // await download_genotype(page, "Initial Genotypes");
  await download_genotype(page, "geno_init_3_allMethods", "hd");
});
test("download_genotypes ld", async ({ page }) => {
  await login(page, user, psw);
  // await download_genotype(page, "Initial Genotypes");
  await download_genotype(page, "geno_init_3_allMethods", "ld");
});
test("download_genotypes snp", async ({ page }) => {
  await login(page, user, psw);
  // await download_genotype(page, "Initial Genotypes");
  await download_genotype(page, "geno_init_3_allMethods", "snp");
});

async function download_genotype(
  page: Page,
  request_name: string,
  type: string,
) {
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();
  await page.getByRole("link", { name: "Genotype data" }).click();

  await page.locator("#geno_requests-selectized").click();
  await page.getByRole("option", { name: request_name }).click();

  const button_id = "#dwnlGeno_" + type;

  await expect(page.locator(button_id)).toBeVisible();
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    await page.locator(button_id).click(),
  ]);

  const downloadPath = await download.path();
  expect(downloadPath).toBeTruthy();
}
