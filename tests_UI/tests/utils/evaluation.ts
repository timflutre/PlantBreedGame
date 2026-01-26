import { expect, Page } from "@playwright/test";

export async function goto_evaluation(page: Page) {
  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
}

export async function expect_eval_submitted_inds_list_to_have(
  page: Page,
  user: string,
  inds: string[],
) {
  await expect(
    page.getByRole("cell", { name: user, exact: true }),
  ).toBeVisible();

  const row = page.locator("tr", {
    has: page.locator("td", { hasText: new RegExp(`^${user}$`) }),
  });
  await expect(row.locator("td").nth(1)).toHaveText(user);
  await expect(row.locator("td").nth(2)).toHaveText(inds.length.toString());
  for (let ind of inds) {
    await expect(row.locator("td").nth(3)).toContainText(ind);
  }
}
