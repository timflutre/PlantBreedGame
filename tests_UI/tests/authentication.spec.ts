import { test, expect } from "@playwright/test";
import { login } from "./utils/authentication";
import { page_root } from "./utils/constants";

const psw: string = "1234";

test.beforeEach(async ({ page }) => {
  await page.goto(page_root);
});

test("basicLogin", async ({ page }) => {
  const username = "admin";
  await login(page, username, psw);
  await expect(page.locator("#breederInfoID-breederBox h3")).toHaveText(
    username,
  );
  await expect(page.locator("#breederInfoID-dateBox p")).toHaveText("Date");
  await expect(page.locator("#breederInfoID-budgetBox p")).toHaveText("Budget");
});
