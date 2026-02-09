import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication.ts";
import { page_root, admin_user, admin_psw } from "./utils/constants.ts";
import { addBreeder, deleteBreeder } from "./utils/breeder-management.ts";

import { check_plotly } from "./utils/plotly_checks.ts";

const psw: string = "data_viz";
const user = "data_viz_breeder";

test.beforeAll(async ({ browser }) => {
  const context = await browser.newContext();
  const page = await context.newPage();
  await page.goto(page_root);
  await login(page, admin_user, admin_psw);
  await addBreeder(page, user, psw, [
    "data_viz",
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

test("data-viz", async ({ page }) => {
  test.setTimeout(60000);
  await login(page, user, psw);

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.getByRole("link", { name: "Phenotypes data" }),
  ).toBeVisible();

  const variables = [
    "-- None --",
    "ind",
    "control_ind",
    "year",
    "plot",
    "pathogen",
    "trait1",
    "trait2",
    "trait3",
  ];

  // check data table
  for (let variable of variables.slice(1)) {
    await expect(
      page
        .locator("#data-viz_pheno-data-table")
        .locator("th")
        .filter({ hasText: new RegExp(`^${variable}$`) }),
    ).toBeVisible();
  }

  // check all variables can be selected for X/Y/Col
  const variable_selection_ids = [
    "#data-viz_pheno-x_var-selectized",
    "#data-viz_pheno-y_var-selectized",
    "#data-viz_pheno-col_var-selectized",
  ];
  for (let select_id of variable_selection_ids) {
    await expect(page.locator(select_id)).toBeVisible();
    await page.locator(select_id).click();
    for (let variable of variables) {
      await expect(
        page.getByRole("option", { name: variable, exact: true }),
      ).toBeVisible();
    }
    await page.getByRole("option", { name: variables[0] }).click();
  }

  var plot = page.locator("#data-viz_pheno-plot div.plotly");

  // check scatter plot (ie. 1 quantitative variables)
  await set_plot_variables({ page, x_var: "trait1" });
  await check_plotly({
    plotly_locator: plot,
    x_axis_title: "trait1",
    y_axis_title: "Number of observations",
  });

  // check scatter plot (ie. 1 qualitative variables)
  await set_plot_variables({ page, x_var: "year" });
  await check_plotly({
    plotly_locator: plot,
    x_axis_title: "year",
    y_axis_title: "Number of observations",
  });

  // check scatter plot (ie. 2 quantitative variables)
  await set_plot_variables({ page, x_var: "trait1", y_var: "trait2" });
  // var plot = page.locator("#data-viz_pheno-plot div.plotly");
  await check_plotly({
    plotly_locator: plot,
    x_axis_title: "trait1",
    y_axis_title: "trait2",
  });

  // check scatter plot (ie. 1 quantitative and 1 qualitative variables)
  await set_plot_variables({ page, x_var: "year", y_var: "trait2" });
  // var plot = page.locator("#data-viz_pheno-plot div.plotly");
  await check_plotly({
    plotly_locator: plot,
    x_axis_title: "year",
    y_axis_title: "trait2",
  });
});

async function set_plot_variables({
  page,
  x_var = "-- None --",
  y_var = "-- None --",
  col_var = "-- None --",
}: {
  page: Page;
  x_var?: string;
  y_var?: string;
  col_var?: string;
}) {
  await page.locator("#data-viz_pheno-x_var-selectized").click();
  await page.getByRole("option", { name: x_var }).click();
  await page.waitForTimeout(500);

  await page.locator("#data-viz_pheno-y_var-selectized").click();
  await page.getByRole("option", { name: y_var }).click();
  await page.waitForTimeout(500);

  await page.locator("#data-viz_pheno-col_var-selectized").click();
  await page.getByRole("option", { name: col_var }).click();
  await page.waitForTimeout(500);
}
