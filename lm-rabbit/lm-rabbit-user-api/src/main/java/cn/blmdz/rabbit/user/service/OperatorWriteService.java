package cn.blmdz.rabbit.user.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.model.Operator;
import cn.blmdz.wolf.user.model.User;

/**
 * @author Effet
 */
public interface OperatorWriteService {

    Response<Long> create(User user, Operator operator);

    Response<Boolean> update(Operator operator);
}
