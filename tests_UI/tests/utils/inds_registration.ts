import { expect, Page } from "@playwright/test";

export async function goto_ind_registration(page: Page) {
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
}

export async function add_registerIndividual(page: Page, inds: string[]) {
  await page.locator(".tab-pane .active").locator(".selectize-input").click();
  for (let ind of inds) {
    await page.keyboard.type(ind);
    await page.keyboard.press("Enter");
  }
  await page.keyboard.press("Escape");
  await page.click("body");
  await page.getByRole("button", { name: "submit" }).click();
}

export async function remove_registerindividual(page: Page, inds: string[]) {
  for (let ind of inds) {
    await expect(page.getByRole("cell", { name: ind })).toBeVisible();
    await page.getByRole("cell", { name: ind }).click();
    await expect(
      page.getByRole("cell", { name: ind }).locator(".."),
    ).toHaveClass(/active/);
  }
  await page.getByRole("button", { name: "Delete" }).click();
}
