import { test } from "@playwright/test";
import { page_root } from "./utils/constants";
import { initialise_game } from "./utils/game_initialisation";

test.beforeEach(async ({ page }) => {
  await page.goto(page_root);
});

test("re-initialisation", async ({ page }) => {
  const to = 200_000;
  test.setTimeout(to);
  await initialise_game({ page, admin_login: true, timeout: to });
});
