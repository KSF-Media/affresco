import React from "react";

const AdvertorialLiftup = (props) => {
  return (
    <a className="block p-3 text-black no-underline bg-advertorial dark:text-aptoma-white"
       href={"/article/" + props.uuid}>
      <div className="pt-1 pb-2 text-xs font-bold font-duplexserif">
        <span className="mr-1 text-gray-500 font-roboto dark:text-aptoma-white">
          {!props.company ? "ANNONS" : "ANNONS:"}
        </span>
        {!props.company ? "" : (
          <span className="mr-1 text-black font-duplexserif dark:text-aptoma-white">
            {props.company.toUpperCase()}
          </span>
        )}
      </div>
      <div className="flex overflow-y-hidden items-center w-full max-h-96">
        <img className="overflow-y-hidden w-auto max-w-full"
             src={props.image}
             alt=""
        />
      </div>
      <h2 className="mt-3 text-3xl font-semibold break-words font-robotoslab">
        {props.listTitle}
      </h2>
    </a>
  );
};

export default AdvertorialLiftup;
