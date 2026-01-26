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

test("Add breeder", async ({ page }) => {
  await login(page, admin_user, admin_psw);
  await addBreeder(page, newBreederName, newBreederPsw, "tester");
  await expect(
    page.getByText("Breeder " + newBreederName + " created"),
  ).toBeVisible();
  await login(page, "newBreeder", newBreederPsw);
});

test("Delete breeder", async ({ page }) => {
  await login(page, admin_user, admin_psw);
  await deleteBreeder(page, newBreederName);
  await expect(
    page.getByText("Breeder " + newBreederName + " deleted"),
  ).toBeVisible();
});

test("Add breeder and just after delete it", async ({ page }) => {
  const breederName = newBreederName + "_2";
  await login(page, admin_user, admin_psw);
  await addBreeder(page, breederName, newBreederPsw, "tester");
  await expect(
    page.getByText("Breeder " + breederName + " created"),
  ).toBeVisible();
  await deleteBreeder(page, breederName);
  await expect(
    page.getByText("Breeder " + breederName + " deleted"),
  ).toBeVisible();
});

// TODO: check we cannot add already existing breeders (low priority)
