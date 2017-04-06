package io.terminus.galaxy.web.admin.role;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Params;
import io.terminus.galaxy.user.model.Operator;
import io.terminus.galaxy.user.service.OperatorReadService;
import io.terminus.galaxy.user.service.OperatorWriteService;
import io.terminus.parana.common.utils.EncryptUtil;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.user.service.UserWriteService;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * @author Effet
 */
@Slf4j
@RestController
@RequestMapping("/api/operator")
public class Operators {

    private final UserReadService<User> userReadService;

    private final UserWriteService<User> userWriteService;

    private final OperatorReadService operatorReadService;

    private final OperatorWriteService operatorWriteService;

    @Autowired
    public Operators(UserReadService<User> userReadService,
                     UserWriteService<User> userWriteService,
                     OperatorReadService operatorReadService,
                     OperatorWriteService operatorWriteService) {
        this.userReadService = userReadService;
        this.userWriteService = userWriteService;
        this.operatorReadService = operatorReadService;
        this.operatorWriteService = operatorWriteService;
    }

    /**
     * ADMIN 创建运营
     *
     * @param operator 运营信息
     * @return 运营用户 ID
     */
    @RequestMapping(value = "", method = RequestMethod.POST)
    public Long createOperator(@RequestBody OperatorPost operator) {
        User user = new User();
        user.setName(operator.getUsername());
        user.setPassword(operator.getPassword());

        Operator op = new Operator();
        op.setRoleId(operator.getRoleId());
        return RespHelper.or500(operatorWriteService.create(user, op));
    }

    @RequestMapping(value = "/{userId}", method = RequestMethod.PUT)
    public Boolean updateOperator(@PathVariable Long userId, @RequestBody OperatorPost operator) {
        Response<Operator> operatorResp = operatorReadService.findByUserId(userId);
        if (!operatorResp.isSuccess()) {
            log.warn("operator find fail, userId={}, error={}", userId, operatorResp.getError());
            throw new JsonResponseException(operatorResp.getError());
        }
        Operator existOp = operatorResp.getResult();

        User toUpdateUser = new User();
        toUpdateUser.setId(userId);
        toUpdateUser.setName(Params.trimToNull(operator.getUsername()));
        String password = Params.trimToNull(operator.getPassword());
        if (password != null) {
            password = EncryptUtil.encrypt(password);
            toUpdateUser.setPassword(password);
        }

        Response<Boolean> userResp = userWriteService.update(toUpdateUser);
        if (!userResp.isSuccess()) {
            log.warn("user update failed, cause:{}", userResp.getError());
            throw new JsonResponseException(userResp.getError());
        }

        if (operator.getRoleId() != null) {
            Operator toUpdateOperator = new Operator();
            toUpdateOperator.setId(existOp.getId());
            toUpdateOperator.setRoleId(operator.getRoleId());
            Response<Boolean> opUpdateResp = operatorWriteService.update(toUpdateOperator);
            if (!opUpdateResp.isSuccess()) {
                log.warn("operator update failed, error={}", opUpdateResp.getError());
                throw new JsonResponseException(opUpdateResp.getError());
            }
        }
        return Boolean.TRUE;
    }

    @RequestMapping(value = "/{userId}/frozen", method = RequestMethod.PUT)
    public Boolean frozenOperator(@PathVariable Long userId) {
        Response<Operator> opResp = operatorReadService.findByUserId(userId);
        if (!opResp.isSuccess()) {
            log.warn("operator find failed, userId={}, error={}", userId, opResp.getError());
            throw new JsonResponseException(opResp.getError());
        }
        Operator op = opResp.getResult();
        if (!op.isActive()) {
            return Boolean.FALSE;
        }

        Operator toUpdate = new Operator();
        toUpdate.setId(op.getId());
        toUpdate.setStatus(0);
        Response<Boolean> updateResp = operatorWriteService.update(toUpdate);
        if (!updateResp.isSuccess()) {
            log.warn("frozen operator failed, userId={}, error={}", userId, updateResp.getError());
            throw new JsonResponseException(updateResp.getError());
        }
        return Boolean.TRUE;
    }

    @RequestMapping(value = "/{userId}/unfrozen", method = RequestMethod.PUT)
    public Boolean unfrozenOperator(@PathVariable Long userId) {
        Response<Operator> opResp = operatorReadService.findByUserId(userId);
        if (!opResp.isSuccess()) {
            log.warn("operator find failed, userId={}, error={}", userId, opResp.getError());
            throw new JsonResponseException(opResp.getError());
        }
        Operator op = opResp.getResult();
        if (!Objects.equals(op.getStatus(), 0)) {
            return Boolean.FALSE;
        }

        Operator toUpdate = new Operator();
        toUpdate.setId(op.getId());
        toUpdate.setStatus(1);
        Response<Boolean> updateResp = operatorWriteService.update(toUpdate);
        if (!updateResp.isSuccess()) {
            log.warn("frozen operator failed, userId={}, error={}", userId, updateResp.getError());
            throw new JsonResponseException(updateResp.getError());
        }
        return Boolean.TRUE;
    }

    @Data
    public static class OperatorPost {

        private String username;

        private String password;

        private Long roleId;
    }
}
