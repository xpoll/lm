package io.terminus.galaxy.web.core.trade.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.JsonMapper;
import io.terminus.galaxy.web.core.trade.engine.ActionRouter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * Mail: F@terminus.io
 * Data: 16/3/4
 * Author: yangzefeng
 */
@RestController
@Slf4j
@RequestMapping("/api/order")
public class CoreWriteOrders {

    private final ActionRouter actionRouter;

    private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

    @Autowired
    public CoreWriteOrders(ActionRouter actionRouter) {
        this.actionRouter = actionRouter;
    }

    @RequestMapping(value = "/update", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Map<String, Object> update(@RequestParam("actionInstanceId") Long actionInstanceId,
                       @RequestParam(value = "updateContext", required = false) String updateContext) {
        Map<String, Object> updateParams = JSON_MAPPER.fromJson(updateContext,
                JSON_MAPPER.createCollectionType(Map.class, String.class, Object.class));

        Response<Map<String, Object>> updateR =
                actionRouter.updateOrder(actionInstanceId, updateParams);
        if (!updateR.isSuccess()) {
            throw new JsonResponseException(updateR.getError());
        }
        return updateR.getResult();
    }
}
