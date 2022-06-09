FROM node:12

RUN ln -sf /usr/share/zoneinfo/Europe/Helsinki /etc/localtime

COPY output /app/output
COPY dist /app/dist
COPY static /app/static
COPY package.json /app/

WORKDIR app
RUN yarn install
CMD node -r dotenv/config -e 'require("./output/Main/index").main()'
