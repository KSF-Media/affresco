var React = require("react");

const ErrorPage = (props) => {
  return (
    <div className={"row"}>
      <div
	className={"col-12 mt-2 mt-5 text-center"}
	style={{ wordWrap: "break-word" }}
      >
	<h2 className={"title"}>{props.message}</h2>
      </div>
    </div>
  );
};

export default ErrorPage;
