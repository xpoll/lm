package io.terminus.galaxy.user.service;

import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.parana.user.model.User;

/**
 * @author Effet
 */
public interface OperatorWriteService {

    Response<Long> create(User user, Operator operator);

    Response<Boolean> update(Operator operator);
}
