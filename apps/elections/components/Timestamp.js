import React from 'react';

import TimeAgo from 'react-timeago'
import swedishStrings from 'react-timeago/lib/language-strings/sv'
import buildFormatter from 'react-timeago/lib/formatters/buildFormatter'

const formatter = buildFormatter(swedishStrings);

export default function Timestamp(props) {
  return !props.timestamp ? <div className="timestamp"></div> : (
    <div className="timestamp">
      <TimeAgo date={props.timestamp} formatter={ formatter }/>
    </div>
  );
}
