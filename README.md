# Three-Party System Game

A classroom game about the health care market. Student teams play hospitals and
insurers. Hospitals and insurers negotiate payment rates, insurers set premiums,
and the game shows who gets coverage, who gets care, and who makes a profit.

**Play it here: https://cellis212.github.io/three-party-game/**

## How it runs

The site is the R Shiny app in `app/`, built with
[shinylive](https://posit-dev.github.io/r-shinylive/). R runs inside the
browser (webR), so there is no server.

- The first load downloads about 100 MB. Later loads use the browser cache.
- The game lives in the browser tab. If you reload or close the tab, the game
  is gone. Use the **Download ... (CSV)** buttons in Instructor Control to keep a copy.
- Each browser tab is a separate game. Run the game from one tab only.
- The pop-out negotiation board is a second window that the instructor's tab
  updates every 3 seconds. Allow pop-ups for the site.

## How to play

1. Open **Instructor Control** and click **Create New Game**.
2. Teams agree on rates and premiums. The instructor types them in.
3. Click **Calculate Round Results**, then **Advance to Next Round**.
4. Project **Public Display** so teams can see the market.

## Updating the site

The app source is kept in a private repository with the instructor materials.
A push to `main` here rebuilds the site with GitHub Actions
(`.github/workflows/deploy.yml`).

To run the app on your own computer instead:

```r
shiny::runApp("app")
```
