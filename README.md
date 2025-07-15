A work in progress attempt to make a dynamic version of my static website.

## Page frontmatter

Frontmatter gives you a place to store extra information in the page, which you can pull out during your rendering of the page yourself, but there are some particular keys that webplats itself recognised and will use:

| Key | Type | Usage |
| --- | --- | --- |
| *title* | string | The title of the page |
| *date* | ISO 8601 date string | The date of publishing - typically used when ordering pages, which is reverse chronological. |
| *draft* | boolean | If both present and true then the page will not be included in routing. |
| *titleimage* | dict | Must contain both an `image` key, which is a relative path for the image, and `alt` key which is the image description for accessibility. This image will be used for thumbnails in the header and in section lists. |
| *synopsis* | string | A short description of the page's contents, typically for section lists. |
| *scripts* | list of strings | List of paths to javascript files that should be loaded in the page |
| *resources* | list of strings | Files besides the index.md that should be presented via URL |
| *content* | boolean | If this key exists and is marked false, then no route to the page will exist, but it will still be listed in the section. Makes it easy to build up sections where you want a list of things but those things shouldn't have a page. |
| *tags* | list of strings | Tags associated with the page. |
| *aliases* | list of strings | Alternative site relative URLs this page can be accessed via. These will generate 302 responses to the canonical version of the page. |
| *in_feed* | boolean | If this key exists and is marked false, the page will not appear in the RSS feed. |

## Shortcodes

Whilst markdown is preferred, there are some [Jekyll](https://jekyllrb.com)/[Hugo](https://gohugo.io) style short codes supported:

| Shortcode | Args | Usage |
| --- | --- | --- |
| audio | filename | Inserts an audio player into the page |
| chart | sytle, CSV filename, x-axis column, y-axis column | Insert a chart using vega-lite into page |
| img | filename, alt text | Inserts a raster image or SVG into the page |
| video | filename, thumbnail image filename | Inserts an HTML5 video player into the page |
| videoloop | filename, thumbnail image filename | Inserts an HTML5 video player into the page configured to loop |
| youtube | video code | Inserts a non-cookie based youtube embed |
