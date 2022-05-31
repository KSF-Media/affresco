FROM node:12

COPY dist /app/dist
COPY package.json /app/

WORKDIR app
RUN npm install

CMD node -r dotenv/config dist/server
