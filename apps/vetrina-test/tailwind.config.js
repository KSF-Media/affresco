/** @type {import('tailwindcss').Config} */
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

const listOfIcons = {
  vetrinaNewPurchase: `url("../../images/subscribe-paywall-icon.svg")`
};

const maskImagePlugin = plugin(
  function ({ matchUtilities, theme }) {
    matchUtilities(
      {
        maskimage: (value) => ({
          maskImage: value,
          maskPosition: "0 0",
          maskRepeat: "no-repeat",
        }),
      },
      { values: theme("maskImage") }
    );
  },

  {
    theme: {
      maskImage: listOfIcons,
    },
  }
);

const maskSizePlugin = plugin(function ({ matchUtilities, theme }) {
  matchUtilities(
    {
      "mask-size": (value) => (
          value == "cover" || value == "contain"
            ? { maskSize: `${value}` }
            : { maskSize: `${value} ${value}` })
    },
    { values: {...theme("spacing"), "cover": "cover", "contain": "contain"} }
  );
});

const maskPositionPlugin = plugin(function ({ matchUtilities }) {
  matchUtilities(
    {
      "mask-position": (value) => ({"mask-position": `${value}`})
    },
    { values: {"center": "center"} }
  );
});

const maskRepeatPlugin = plugin(function ({ addUtilities, matchUtilities }) {
  addUtilities({
    '.mask-repeat': {
      'mask-repeat': 'repeat',
    },
  });
  matchUtilities(
    {
      "mask-repeat": (value) => ({ "mask-repeat": `${value}` }),
    },
    {
      values: {
        none: "no-repeat",
        x: "repeat-x",
        y: "repeat-y",
      },
    }
  );
});


module.exports = {
  content: {
    files: ["./src/**/*", "../../packages/vetrina/src/**/*"],
    transform: {
      // We have to use \\_ instead of \_ in purs files
      purs: (content) => content.replace(/\\\\/g, "\\"),
    },
  },
  theme: {
    extend: {},
    colors: {
      transparent: "transparent",
      gray: {
        50: "#f7f5f3", // light
        75: "#eceae6", // nude
        100: "#e0e0e0", // hairline-color
        200: "#dcd9d7", // almostlight
        300: "#cbcaca", // slightlylight
        400: "#999999", // mediumlight
        500: "#6d7573", // medium
        600: "#575d5c", // mediumdark
        700: "#535251", // warm-mid
        800: "#333333", // dark
        850: "#313639", // text-bg-dark
        900: "#141414", // dark-text
        910: "#121314", // bg-dark
        950: "#0f1011", // deepdark
      },
      white: "#ffffff",
      black: "#000000",
      blue: {
        link: "#00698e",
      },
      green: {
        300: "#249c25", // button-color-primary-hover
        500: "#00810a", // button-color-primary
      },
      hbl: "#f07e26",
      on: "#518196",
      vn: "#c90c0f",
      neutral: "#00a1ab",
      brand: "var(--brand-color)", // defined in _site.scss
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
  plugins: [maskImagePlugin, maskSizePlugin, maskPositionPlugin, maskRepeatPlugin],
}

