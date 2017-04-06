package io.terminus.galaxy.web.admin.order;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.galaxy.order.service.OrderCommentWriteService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Author:cp
 * Created on 4/26/16.
 */
@Slf4j
@RestController
@RequestMapping("/api/admin/orders")
public class Comments {

    @Autowired
    private OrderCommentWriteService orderCommentWriteService;

    @RequestMapping(value = "/comment", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    public void batchUpdateComment(@RequestParam("ids") String idStr,
                                   @RequestParam("status") Integer status) {
        List<Long> ids = Splitters.splitToLong(idStr, Splitters.COMMA);

        Response<Boolean> result = orderCommentWriteService.batchUpdateOrderComment(ids, status);
        if (!result.isSuccess()) {
            log.error("fail to batch update order comment by ids {}, status {}, error code:{}",
                    ids, status, result.getError());
            throw new JsonResponseException(result.getError());
        }
    }

    @RequestMapping(value = "/comment/{id}", method = RequestMethod.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
    public void deleteComment(@PathVariable("id") Long id) {
        Response<Boolean> result = orderCommentWriteService.deleteOrderComment(id);
        if (!result.isSuccess()) {
            log.error("fail to delete order comment where id={}, error code:{}", id, result.getError());
            throw new JsonResponseException(result.getError());
        }
    }

}
