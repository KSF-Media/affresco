FROM node:14

COPY dist /app/dist
COPY package.json /app/

WORKDIR app
RUN yarn install

CMD node -r dotenv/config dist/server
