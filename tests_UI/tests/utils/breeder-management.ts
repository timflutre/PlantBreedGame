import { expect, Page } from "@playwright/test";

export async function addBreeder(
  page: Page,
  breederName: string,
  password: string,
  status: string,
) {
  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(
    page.getByRole("link", { name: "Manage breeders" }),
  ).toBeVisible();
  await page.getByRole("link", { name: "Manage breeders" }).click();

  await expect(page.locator("#newBreederName")).toBeVisible();
  await page.locator("#newBreederName").click();
  await page.locator("#newBreederName").fill(breederName);

  await page.click("#newBreederStatus-selectized");
  await page.getByRole("option", { name: status }).click();

  await page.getByLabel("Password", { exact: true }).click();
  await page.getByLabel("Password", { exact: true }).fill(password);
  await page.getByRole("button", { name: "Add this new breeder" }).click();
}

export async function deleteBreeder(page: Page, breederName: string) {
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

  await expect(
    page.getByText("Breeder " + breederName + " deleted"),
  ).toBeVisible();
}
