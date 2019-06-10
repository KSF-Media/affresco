var merge = require('webpack-merge');
const {
    override,
    disableEsLint,
    addLessLoader,
} = require("customize-cra");

module.exports = override(
    disableEsLint(), addLessLoader(), function (config) {
        var newConfig = merge(config, {
            module: {
                rules: [
                    {
                        parser: {
                            amd: false
                        }
                    }
                ]
            }
        });
        return newConfig;
    }
);