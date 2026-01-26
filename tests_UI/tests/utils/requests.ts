import { expect, Page } from "@playwright/test";

import path from "path";

export async function requestPhenotyping(page: Page, reqFile: string) {
  await page
    .getByRole("link", { name: "flask icon Request phenotyping" })
    .click();

  await page
    .locator("#pheno_file")
    .getByText("Browse...")
    .setInputFiles(path.join("./tests/requestExamples", reqFile));

  await page
    .locator("#pheno_tabset")
    .getByRole("link", { name: "Check" })
    .click();
  await expect(page.locator("#PhenoUploaded")).toContainText("GOOD");

  await page
    .locator("#pheno_tabset")
    .getByRole("link", { name: "Summary" })
    .click();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#PhenoInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page
    .locator("#pheno_tabset")
    .getByRole("link", { name: "Data" })
    .click();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("ind: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("task: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPheno")
      .getByLabel("details: activate to sort column ascending"),
  ).toBeVisible();

  await page
    .locator("#pheno_tabset")
    .getByRole("link", { name: "Request", exact: true })
    .click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();

  await expect(
    page.getByText("Phenotyping request successfully sent"),
  ).toBeVisible();
  await page.locator("div.shiny-notification-close").click();

  await expect(page.locator("#PhenoUploaded")).toHaveText("No file uploaded");
}

export async function requestGenotyping(page: Page, reqFile: string) {
  await page.getByRole("link", { name: "dna icon Request genotyping" }).click();

  await page
    .locator("#geno_file")
    .getByText("Browse...")
    .setInputFiles(path.join("./tests/requestExamples", reqFile));

  await page
    .locator("#geno_tabset")
    .getByRole("link", { name: "Check" })
    .click();
  await expect(page.locator("#GenoUploaded")).toContainText("GOOD");

  await page
    .locator("#geno_tabset")
    .getByRole("link", { name: "Summary" })
    .click();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#GenoInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page
    .locator("#geno_tabset")
    .getByRole("link", { name: "Data" })
    .click();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("ind: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("task: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryGeno")
      .getByLabel("details: activate to sort column ascending"),
  ).toBeVisible();

  await page
    .locator("#geno_tabset")
    .getByRole("link", { name: "Request", exact: true })
    .click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();

  await expect(
    page.getByText("Genotyping request successfully sent"),
  ).toBeVisible();
  await page.locator("div.shiny-notification-close").click();

  await expect(page.locator("#GenoUploaded")).toHaveText("No file uploaded");
}

export async function requestPlantMaterial(page: Page, reqFile: string) {
  // plant material small request
  await page
    .getByRole("link", { name: "seedling icon Request plant material" })
    .click();

  await page
    .locator("#cross_file")
    .getByText("Browse...")
    .setInputFiles(path.join("./tests/requestExamples", reqFile));

  await page
    .locator("#cross_tabset")
    .getByRole("link", { name: "Check" })
    .click();
  await expect(page.locator("#plmatUploaded")).toContainText("GOOD");

  await page
    .locator("#cross_tabset")
    .getByRole("link", { name: "Summary" })
    .click();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Task" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Unitary_Price" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").getByRole("cell", { name: "Quantity" }),
  ).toBeVisible();
  await expect(
    page.locator("#PltmatInvoice").locator("th").filter({ hasText: "Total" }),
  ).toBeVisible();

  await page
    .locator("#cross_tabset")
    .getByRole("link", { name: "Data" })
    .click();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("parent1: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("parent2: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("child: activate to sort column ascending"),
  ).toBeVisible();
  await expect(
    page
      .locator("#qryPlmat")
      .getByLabel("explanations: activate to sort column ascending"),
  ).toBeVisible();

  await page
    .locator("#cross_tabset")
    .getByRole("link", { name: "Request", exact: true })
    .click();
  await expect(page.getByRole("button", { name: "Yes, I do!" })).toBeEnabled();
  await page.getByRole("button", { name: "Yes, I do!" }).click();

  await expect(
    page.getByText("Plant-material request successfully sent"),
  ).toBeVisible();
  await page.locator("div.shiny-notification-close").click();

  await expect(page.locator("#plmatUploaded")).toHaveText("No file uploaded");
}

export async function wait_request_execution(page: Page, request_name: string) {
  await page
    .getByRole("link", { name: "house-user icon Identification / Home" })
    .click();

  await page.getByRole("link", { name: "Requests Progress/History" }).click();

  await expect(async () => {
    await page
      .locator('#requests_history_div .dataTables_filter input[type="search"]')
      .fill(request_name);
    await page.waitForTimeout(200);

    const row = page.locator("#requests_history_DT tbody tr").filter({
      has: page
        .locator("td")
        .nth(1)
        .filter({ hasText: new RegExp(`^${request_name}$`) }),
    });

    const rowCount = await row.count();
    if (rowCount === 0) {
      throw new Error(`No rows found for request: ${request_name}`);
    }

    const status = await row.locator("td").first().textContent();
    const isCompleted = status === "✅ Completed";
    await page
      .locator('#requests_history_div .dataTables_filter input[type="search"]')
      .clear();
    expect(isCompleted).toBeTruthy();
  }).toPass({ timeout: 120000 });
}
