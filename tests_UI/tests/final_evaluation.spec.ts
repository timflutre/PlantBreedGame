import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import { admin_user, admin_psw } from "./utils/constants";
import { readFileSync } from "fs";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { requestPlantMaterial, wait_request_execution } from "./utils/requests";
import {
  goto_ind_registration,
  add_registerIndividual,
} from "./utils/inds_registration.ts";
import {
  goto_evaluation,
  expect_eval_submitted_inds_list_to_have,
} from "./utils/evaluation.ts";

import { check_plotly } from "./utils/plotly_checks.ts";

const inds_to_register = {
  eval: [
    "G1_set10_01",
    "G1_set10_02",
    "G1_set10_03",
    "G1_set10_04",
    "G1_set10_05",
  ],
  eval_2: [
    "G1_set10_10",
    "G1_set10_09",
    "G1_set10_08",
    "G1_set10_07",
    "G1_set10_06",
  ],
};

test.describe.configure({ mode: "serial" });

test.beforeAll(async ({ browser }) => {
  test.setTimeout(60000);
  const context = await browser.newContext();
  const page = await context.newPage();
  await login(page, admin_user, admin_psw);
  for (const user in inds_to_register) {
    await addBreeder(page, user, user, [
      "evaluation",
      "no_time_constraint",
      "no_request_size_constraint",
    ]);
  }
  for (const user in inds_to_register) {
    await login(page, user, user);
    await requestPlantMaterial(page, "pltMat_init_set10.txt");
  }
  for (const user in inds_to_register) {
    // separated loops to give time for the request to be processed
    await login(page, user, user);
    await wait_request_execution(page, "pltMat_init_set10");
    await login(page, user, user); // needed to be sure the individuals
    // registration input is updated with the newlly created individuals
    // TODO: this should be improved on the app
    await goto_ind_registration(page);
    const new_inds = inds_to_register[user];
    await add_registerIndividual(page, new_inds);
  }
  await context.close();
});

test.afterAll(async ({ browser }) => {
  // test.setTimeout(30000);
  // const context = await browser.newContext();
  // const page = await context.newPage();
  // await login(page, admin_user, admin_psw);
  // for (const user in inds_to_register) {
  //   await deleteBreeder(page, user);
  // }
  // await context.close();
});

test("run evaluation", async ({ page }) => {
  test.setTimeout(60000);

  let registered_inds = structuredClone(inds_to_register);
  // registered_inds["Controls"] = Array(5).fill("Coll");
  registered_inds["Controls"] = [
    "Coll0163",
    "Coll0389",
    "Coll0598",
    "Coll0611",
    "Coll0758",
  ];

  await login(page, admin_user, admin_psw);
  await goto_evaluation(page);

  for (const user in registered_inds) {
    await expect_eval_submitted_inds_list_to_have(
      page,
      user,
      registered_inds[user],
    );
  }

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await page.getByRole("button", { name: "Launch evaluation!" }).click();
  await expect(page.locator("#evalGraphT1")).toBeVisible();

  // check T1
  await expect(page.locator("#evalGraphT1")).toBeVisible();
  const T1_plot = page.locator("#evalGraphT1 div.plotly");
  await check_plotly({
    plotly_locator: T1_plot,
    title: "Phenotypic values of trait 1",
    legend_elements: Object.keys(registered_inds),
    x_axis_elements: Object.entries(inds_to_register).flatMap(([key, values]) =>
      values.map((value) => `${key} ${value}`),
    ),
  });

  // check T2
  await page.getByRole("link", { name: "Trait 2" }).click();
  await expect(page.locator("#evalGraphT2")).toBeVisible();
  const T2_plot = page.locator("#evalGraphT2 div.plotly");
  await check_plotly({
    plotly_locator: T2_plot,
    title: "Phenotypic values of trait 2",
    legend_elements: Object.keys(registered_inds),
    x_axis_elements: Object.entries(inds_to_register).flatMap(([key, values]) =>
      values.map((value) => `${key} ${value}`),
    ),
  });

  // check T3
  await page.getByRole("link", { name: "Trait 3" }).click();
  await expect(page.locator("#evalGraphT3")).toBeVisible();
  const T3_plot = page.locator("#evalGraphT3 div.plotly");
  await check_plotly({
    plotly_locator: T3_plot,
    title: "Phenotypic values of trait 3",
    legend_elements: Object.keys(registered_inds),
    x_axis_elements: Object.entries(inds_to_register).flatMap(([key, values]) =>
      values.map((value) => `${key} ${value}`),
    ),
  });

  // check T1xT2
  await page.getByRole("link", { name: "Traits 1 vs 2" }).click();
  await expect(page.locator("#evalGraphT1vT2")).toBeVisible();
  await expect(page.locator("#evalGraphT1vT2 div.plotly")).toBeVisible();
  const T1xT2_plot = page.locator("#evalGraphT1vT2 div.plotly");
  await check_plotly({
    plotly_locator: T1xT2_plot,
    title: "Genotypic values (+intercept) of traits 1 vs 2",
    legend_elements: Object.keys(registered_inds).concat([
      "Linear regression",
      "Initial Collection",
    ]),
    x_axis_title: "GA 1",
    y_axis_title: "GA 2",
  });

  // check pedigree
  await page.getByRole("link", { name: "Pedigree" }).click();
  await expect(page.locator("#evalUIpedigree")).toBeVisible();
  await expect(page.locator("#evalPlotPedigree")).toBeVisible();

  await expect(page.locator("#evalUIpedigree .selectize-input")).toBeVisible();
  for (let breeder in inds_to_register) {
    await page.locator("#evalUIpedigree .selectize-input").click();
    await expect(
      page
        .locator("#evalUIpedigree .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIpedigree .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    await expect(page.getByRole("img", { name: "Plot object" })).toBeVisible();
  }

  // check allele frequencies
  await page.getByRole("link", { name: "AFs" }).click();
  await expect(page.locator("#evalUIAfsPlot")).toBeVisible();

  await expect(page.locator("#propAFS")).toBeVisible();
  await expect(page.locator("#evalUIAfsPlot .selectize-input")).toBeVisible();
  for (let breeder in inds_to_register) {
    await page.locator("#evalUIAfsPlot .selectize-input").click();
    await expect(
      page
        .locator("#evalUIAfsPlot .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIAfsPlot .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();

    let afs_plot1 = page.locator("#evalGraphAFsHist .plotly");
    await check_plotly({
      plotly_locator: afs_plot1,
      title: breeder,
      legend_elements: ["initial AFs", "final AFs"],
      x_axis_title: "allele frequencies",
      timeout: 10000,
    });

    let afs_plot2 = page.locator("#evalGraphAFsScatter .plotly");
    await check_plotly({
      plotly_locator: afs_plot2,
      title: breeder,
      legend_elements: ["trace 1"],
      x_axis_title: "initial allele frequencies",
      y_axis_title: "final allele frequencies",
      timeout: 10000,
    });
  }

  // check additive relationships
  await page.getByRole("link", { name: "Additive relationships" }).click();
  await expect(page.locator("#evalUIaddRelation")).toBeVisible();
  await expect(page.locator("#addRelTable")).toBeVisible();

  await expect(
    page.locator("#evalUIaddRelation .selectize-input"),
  ).toBeVisible();
  for (let breeder in inds_to_register) {
    await page.locator("#evalUIaddRelation .selectize-input").click();
    await expect(
      page
        .locator("#evalUIaddRelation .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIaddRelation .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    await expect(page.locator("#addRelTable")).toBeVisible();
  }

  // check request history
  await page.getByRole("link", { name: "Requests history" }).click();
  await expect(page.locator("#evalUIrequestHistory")).toBeVisible();

  await expect(
    page.locator("#evalUIrequestHistory .selectize-input"),
  ).toBeVisible();

  for (let breeder of Object.keys(inds_to_register).concat([
    "--- All Breeders ---",
  ])) {
    await page.locator("#evalUIrequestHistory .selectize-input").click();
    await expect(
      page
        .locator("#evalUIrequestHistory .selectize-dropdown")
        .getByRole("option", { name: breeder, exact: true }),
    ).toBeVisible();
    await page
      .locator("#evalUIrequestHistory .selectize-dropdown")
      .getByRole("option", { name: breeder, exact: true })
      .click();
    // await expect(page.locator("#historyTable")).toBeVisible();

    let history_plot = page.locator("#historyTable .plotly");
    await check_plotly({
      plotly_locator: history_plot,
    });
  }

  // check players' scores
  await page.getByRole("link", { name: "Players' scores" }).click();
  await expect(page.locator("#evalUIgameScores")).toBeVisible();
  await expect(
    page.locator("#evalUIgameScores .selectize-input"),
  ).toBeVisible();

  for (let score of ["T1 with sufficient quality", "Product T1 x T2"]) {
    await page.locator("#evalUIgameScores .selectize-input").click();

    await page
      .locator("#evalUIgameScores .selectize-dropdown")
      .getByRole("option", { name: score })
      .click();
    await expect(page.locator("#averageScore")).toBeVisible();
    await expect(page.locator("#T3_penalty-label")).toBeVisible();
    if (score === "T1 with sufficient quality") {
      await expect(page.locator("#evalUI_t2penalty")).toBeVisible();
    }

    page.getByRole("button", { name: "medal icon Find the Winner !" }).click();
    await expect(page.locator("#podium")).toBeVisible();
    await expect(page.locator("#scoreTable")).toBeVisible();
    await expect(page.locator("#scoreTable thead")).toHaveText(/breeder/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/ind/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/score/);

    for (let breeder in registered_inds) {
      await expect(
        page.locator("#podium ol").getByText(breeder, { exact: true }),
      ).toBeVisible();

      for (let ind of registered_inds[breeder]) {
        await expect(
          page.getByRole("cell", { name: breeder + " " + ind }),
        ).toBeVisible();
      }
    }

    await page.getByLabel("Average accross all submited individuals").click();
    page.getByRole("button", { name: "medal icon Find the Winner !" }).click();
    await expect(page.locator("#scoreTable")).toBeVisible();

    await expect(
      page.locator("#scoreTable thead").getByText(/ind/),
    ).toHaveCount(0);
    await expect(page.locator("#scoreTable thead")).toHaveText(/breeder/);
    await expect(page.locator("#scoreTable thead")).toHaveText(/score/);
    for (let breeder in registered_inds) {
      await expect(
        page
          .locator("#scoreTable")
          .getByRole("cell", { name: breeder, exact: true }),
      ).toBeVisible();
    }
    await page.getByLabel("Average accross all submited individuals").click();
  }

  //check download report
  await page.getByRole("link", { name: "Download Report" }).click();
  await expect(page.locator("#elvalReport")).toBeVisible();
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    await page.locator("#elvalReport").click(),
  ]);

  expect(readFileSync(await download.path(), { encoding: "utf8" })).toContain(
    "Breeding Game, Final Results",
  );
});
