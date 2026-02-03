import { expect, Locator } from "@playwright/test";

export async function check_plotly({
  plotly_locator,
  title = "",
  legend_elements = [],
  legend_title = "",
  x_axis_elements = [],
  x_axis_title = "",
  y_axis_title = "",
  timeout = 5000,
}: {
  plotly_locator: Locator;
  title?: string;
  legend_elements?: Array<string>;
  legend_title?: string;
  x_axis_elements?: Array<string>;
  x_axis_title?: string;
  y_axis_title?: string;
  timeout?: number;
}) {
  await expect(plotly_locator).toBeVisible({ timeout: timeout });

  if (title) {
    const plotTitle = plotly_locator.locator(".g-gtitle .gtitle");
    await expect(plotTitle).toHaveAttribute("data-unformatted", title);
  }

  // legend:
  const legend = plotly_locator.locator("g.legend");
  if (legend_title) {
    await expect(
      legend.locator("text.legendtitletext", {
        hasText: new RegExp(`^${legend_title}$`),
      }),
    ).toBeVisible();
  }
  for (const legend_elem of legend_elements) {
    await expect(
      legend.locator("text.legendtext", {
        hasText: new RegExp(`^${legend_elem}$`),
      }),
    ).toBeVisible();
  }

  const xAxisLayer = plotly_locator.locator("g.xaxislayer-above");
  for (const x_elem of x_axis_elements) {
    await expect(
      xAxisLayer.locator("text", {
        hasText: new RegExp(`^${x_elem}$`),
      }),
    ).toBeVisible();
  }

  if (x_axis_title) {
    const xAxisTitle = plotly_locator.locator(".g-xtitle .xtitle");
    await expect(xAxisTitle).toHaveAttribute("data-unformatted", x_axis_title);
  }

  if (y_axis_title) {
    const yAxisTitle = plotly_locator.locator(".g-ytitle .ytitle");
    await expect(yAxisTitle).toHaveAttribute("data-unformatted", y_axis_title);
  }
}
