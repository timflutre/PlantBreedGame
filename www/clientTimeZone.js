$(document).on("shiny:connected", function () {
  Shiny.setInputValue(
    "client_time_zone",
    Intl.DateTimeFormat().resolvedOptions().timeZone,
  );
});
