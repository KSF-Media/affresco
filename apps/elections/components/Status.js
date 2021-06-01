import React from "react";
import PieChart from "react-minimal-pie-chart";

const Status = (props) =>
  !props.percentage ? (
    <div className="status"></div>
  ) : (
    <div className="status">
      <div className="percentage">
        <PieChart
          data={[
            {
              value: props.percentage,
              color: "#222",
            },
            {
              value: 100 - props.percentage,
              color: "#666",
            },
          ]}
          startAngle={-90}
        />
        <div className="status-text">
          {props.percentage}%<span>räknat</span>
        </div>
      </div>
    </div>
  );

export default Status;
