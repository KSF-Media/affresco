import React from 'react';

const AreaInfo = (props) => {
  if (props.areaResponse ) {
    const areaInfo = props.areaResponse.area.info;
    let name = areaInfo.name.swedish || areaInfo.name.finnish;
    if (areaInfo.identifier === 'MAA') {
      name = name.toLowerCase();
    }
    return (
      <div className="area-info">
        <h2>Röstfördelning,
          <span className="area-name"> {name}.</span><br />
          <span className="area-votestatus">{areaInfo.calculationStatus}% av rösterna räknade.</span>
        </h2>
      </div>
    )
  }
  return null;
};

export default AreaInfo;
