import { test, expect, Page } from "@playwright/test";
import { login } from "./utils/authentication";
import { page_root, admin_psw, admin_user } from "./utils/constants";
import { addBreeder, deleteBreeder } from "./utils/breeder-management";
import { requestPlantMaterial, wait_request_execution } from "./utils/requests";
import {
  goto_ind_registration,
  add_registerIndividual,
  remove_registerindividual,
} from "./utils/inds_registration.ts";
import {
  goto_evaluation,
  expect_eval_submitted_inds_list_to_have,
} from "./utils/evaluation.ts";

const psw: string = "final";
const user = "final_eval";

test.describe.configure({ mode: "serial" });

test.beforeAll(async ({ browser }) => {
  test.setTimeout(30000);
  const context = await browser.newContext();
  const page = await context.newPage();
  await login(page, admin_user, admin_psw);
  await addBreeder(page, user, psw, "tester");
  await login(page, user, psw);
  await requestPlantMaterial(page, "pltMat_init_set10.txt");
  await wait_request_execution(page, "pltMat_init_set10");
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

test("register individuals", async ({ page }) => {
  await login(page, user, psw);
  await goto_ind_registration(page);
  const new_inds = [
    "G1_set10_01",
    "G1_set10_02",
    "G1_set10_03",
    "G1_set10_04",
    "G1_set10_05",
  ];
  await add_registerIndividual(page, new_inds);
  for (let ind of new_inds) {
    await expect(page.getByRole("cell", { name: ind })).toBeVisible();
  }

  await goto_evaluation(page);
  await expect_eval_submitted_inds_list_to_have(page, user, new_inds);
});

test("delete registered individuals", async ({ page }) => {
  await login(page, user, psw);
  await goto_ind_registration(page);
  const inds_to_remove = ["G1_set10_01", "G1_set10_02", "G1_set10_05"];
  await remove_registerindividual(page, inds_to_remove);
  for (let ind of inds_to_remove) {
    await expect(page.getByRole("cell", { name: ind })).toHaveCount(0);
  }
});

test("add and delete registered individuals", async ({ page }) => {
  await login(page, user, psw);
  await goto_ind_registration(page);

  const new_inds = ["G1_set10_01", "G1_set10_02"];
  await add_registerIndividual(page, new_inds);
  for (let ind of new_inds) {
    await expect(page.getByRole("cell", { name: ind })).toBeVisible();
  }

  await remove_registerindividual(page, new_inds);
  for (let ind of new_inds) {
    await expect(page.getByRole("cell", { name: ind })).toHaveCount(0);
  }
});

test("cannot register more than 5 individuals", async ({ page }) => {
  await login(page, user, psw);
  await goto_ind_registration(page);

  const alertPromise = page.waitForEvent("dialog");
  await add_registerIndividual(page, [
    "G1_set10_06",
    "G1_set10_07",
    "G1_set10_08",
    "G1_set10_09",
  ]);

  const alert = await alertPromise;
  expect(alert.message()).toBe(
    "Sorry, you have already submitted 2 individuals, on a total of 5 . You can only submit 3 more individuals.",
  );
  await alert.accept();
});
