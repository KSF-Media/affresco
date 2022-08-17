const plugin = require("tailwindcss/plugin");

const sansFonts = [
  "ui-sans-serif",
  "system-ui",
  "-apple-system",
  "BlinkMacSystemFont",
  '"Segoe UI"',
  "Roboto",
  '"Helvetica Neue"',
  "Arial",
  '"Noto Sans"',
  "sans-serif",
  '"Apple Color Emoji"',
  '"Segoe UI Emoji"',
  '"Segoe UI Symbol"',
  '"Noto Color Emoji"',
];

const serifFonts = ["ui-serif", "Georgia", "Cambria", '"Times New Roman"', "Times", "serif"];

const monoFonts = [
  "ui-monospace",
  "SFMono-Regular",
  "Menlo",
  "Monaco",
  "Consolas",
  '"Liberation Mono"',
  '"Courier New"',
  "monospace",
];

const maskImagePlugin = plugin(
  function ({ matchUtilities, theme }) {
    matchUtilities(
      {
        maskimage: (value) => ({
          maskImage: value,
          maskPosition: "0 0",
        }),
      },
      { values: theme("maskImage") }
    );
  },

  {
    theme: {
      maskImage: {
        hbl: `url(${__dirname}/../../images/logo-hbl.svg)`,
        vn: `url(${__dirname}/../../images/logo-vn.svg)`,
        on: `url(${__dirname}/../../images/logo-on.svg)`,
      },
    },
  }
);

const maskSizePlugin = plugin(function ({ matchUtilities, theme }) {
  matchUtilities(
    {
      "mask-size": (value) => ({
        maskSize: `${value} ${value}`,
      }),
    },
    { values: theme("spacing") }
  );
});

module.exports = {
  content: {
    files: ["./src/**/*", "./static/**/*"],
    transform: {
      // We have to use \\_ instead of \_ in purs files
      purs: (content) => content.replace(/\\\\/g, '\\')
    },
  },
  theme: {
    extend: {},
    colors: {
      gray: {
        50: "#f7f5f3", // light
        100: "#e0e0e0", // hairline-color
        200: "#dcd9d7", // almostlight
        300: "#cbcaca", // slightlylight
        400: "#999999", // mediumlight
        500: "#6d7573", // medium
        600: "#575d5c", // mediumdark
        700: "#535251", // warm-mid
        800: "#333333", // dark
        900: "#141414", // dark-text
        950: "#0f1011", // deepdark
      },
      white: "#ffffff",
      black: "#000000",
      blue: {
        link: "#00698e",
      },
      hbl: "#f07e26",
      on: "#518196",
      vn: "#c90c0f",
      brand: "var(--brand-color)", // defined in _site.scss
      advertorial: "var(--color-advertorial, #fff3e6)", // defined in Aptoma's CSS
    },

    fontFamily: {
      roboto: ["Roboto", ...sansFonts],
      robotoslab: ["Roboto Slab", ...sansFonts],
      duplexsans: ['"Duplex Sans"', ...sansFonts],
      duplexserif: ['"Duplex Serif"', ...serifFonts],
      mono: ["ui-monospace", ...monoFonts],
    },

    screens: {
      sm: "320px", // @content-block
      md: "760px", // @breakpoint-2col
      lg: "1020px", // @breakpoint-3col
    },
  },
  plugins: [maskImagePlugin, maskSizePlugin],
};
