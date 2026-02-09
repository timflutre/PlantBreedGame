import { test, expect } from "@playwright/test";
import { login } from "./utils/authentication";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { page_root, admin_user, admin_psw } from "./utils/constants";

const newBreederPsw: string = "1234";
const newBreederName: string = "newBreeder";

test.beforeEach(async ({ page }) => {
  await page.goto(page_root);
});

test.describe.configure({ mode: "serial" });

test("breeder without any permission", async ({ page }) => {
  const breederName = newBreederName + "_no_permission";
  const premissions = [];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();

  await login(page, breederName, newBreederPsw);

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toBeVisible();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toHaveText("Content unavailable");
});

test("breeder with admin permission", async ({ page }) => {
  const breederName = newBreederName + "_admin";
  const premissions = ["admin"];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();

  await login(page, breederName, newBreederPsw);

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(page.locator("#admin_tabset")).toBeVisible();

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toBeVisible();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toHaveText("Content unavailable");
});

test("breeder with evaluation permission", async ({ page }) => {
  const breederName = newBreederName + "_eval";
  const premissions = ["evaluation"];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();

  await login(page, breederName, newBreederPsw);

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await expect(
    page.getByRole("button", { name: "Launch evaluation!" }),
  ).toBeVisible();

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toBeVisible();
  await expect(
    page.locator("#data_viz_UI .box-header h3.box-title"),
  ).toHaveText("Content unavailable");
});

test("breeder with data-viz permission", async ({ page }) => {
  const breederName = newBreederName + "_data_viz";
  const premissions = ["data_viz"];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();

  await login(page, breederName, newBreederPsw);

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#adminUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toBeVisible();
  await expect(page.locator("#evalUI .box-header h3.box-title")).toHaveText(
    "Content unavailable",
  );

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.getByRole("link", { name: "Phenotypes data" }),
  ).toBeVisible();
});

test("breeder with all permission", async ({ page }) => {
  const breederName = newBreederName + "_all_perm";
  const premissions = [
    "admin",
    "evaluation",
    "data_viz",
    "no_time_constraint",
    "no_request_size_constraint",
  ];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();

  await login(page, breederName, newBreederPsw);

  await page.getByRole("link", { name: "gears icon Admin" }).click();
  await expect(page.locator("#admin_tabset")).toBeVisible();

  await page.getByRole("link", { name: "medal icon Evaluation" }).click();
  await expect(
    page.getByRole("button", { name: "Launch evaluation!" }),
  ).toBeVisible();

  await page.getByRole("link", { name: "chart-line icon Data" }).click();
  await expect(
    page.getByRole("link", { name: "Phenotypes data" }),
  ).toBeVisible();
});

test("Delete breeder", async ({ page }) => {
  const breederName = newBreederName + "_to_be_removed";
  const premissions = ["admin", "evaluation", "data_viz"];
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, premissions);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();
  await login(page, breederName, newBreederPsw);

  await login(page, admin_user, admin_psw);
  await deleteBreeder(page, breederName);
  await expect(
    page.getByText("Breeder " + breederName + " deleted"),
  ).toBeVisible();
});

test("Add breeder and just after delete it", async ({ page }) => {
  const breederName = newBreederName + "_2";
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, ["admin"]);
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();
  await deleteBreeder(page, breederName);
  await expect(
    page.getByText("Breeder " + breederName + " deleted"),
  ).toBeVisible();
});

// TODO: check we cannot add already existing breeders (low priority)
