import { expect, Page } from "@playwright/test";
import { admin_user, admin_psw } from "./constants.ts";
import { login } from "./authentication.ts";

export async function initialise_game({
  page,
  admin_login = true,
  timeout = 200_000,
}: {
  page: Page;
  admin_login: boolean;
  timeout?: number;
}) {
  if (admin_login) {
    await login(page, admin_user, admin_psw);
  }

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  if (admin_login) {
    await page.getByRole("link", { name: "Game Initialisation" }).click();
    await page.getByLabel("Confirmation:").fill("plantbreedgame");
  }
  await page.locator("#initialiseGame").click();
  await expect(
    page.getByRole("heading", {
      name: "Apimeta simulans , a species with a bright future!",
    }),
  ).toBeVisible({
    timeout: timeout, // game initialisation can be quite long
  });
}
