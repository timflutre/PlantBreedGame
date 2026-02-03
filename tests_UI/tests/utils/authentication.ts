import { expect, Page } from "@playwright/test";
import { page_root } from "./constants";

export async function login(page: Page, username: string, password: string) {
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

  await expect(page.locator("#breederInfoID-breederBox h3")).toHaveText(
    username,
  );
}
