import React, { Component } from 'react';
import ObjectType from './ObjectType';

class ErrorType extends Component {
  constructor(props) {
    super(props);

    this.state = {
      open: props.open,
    };
  }

  render() {
    const { value, shallow = true, filter, allowOpen } = this.props;
    const { open } = this.state;

    const sig = value.name || value.constructor.name;

    return (
      <ObjectType
        filter={filter}
        allowOpen={false}
        type="error"
        shallow={shallow}
        open={false}
        value={value}
        displayName={sig}
      />
    );
  }
}

export default ErrorType;
