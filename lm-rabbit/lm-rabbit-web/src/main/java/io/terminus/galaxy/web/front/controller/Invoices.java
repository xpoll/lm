package io.terminus.galaxy.web.front.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.model.UserVatInvoice;
import io.terminus.galaxy.order.service.UserVatInvoiceReadService;
import io.terminus.galaxy.order.service.UserVatInvoiceWriteService;
import io.terminus.pampas.common.UserUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Desc: 发票相关
 * Mail: hehaiyang@terminus.io
 * Date: 16/3/17
 */
@Slf4j
@Controller
@RequestMapping("/api/user/invoice")
public class Invoices {

    @Autowired
    private UserVatInvoiceReadService userVatInvoiceReadService;
    @Autowired
    private UserVatInvoiceWriteService userVatInvoiceWriteService;

    /**
     * 创建增值税发票定义
     * @param userVatInvoice    增值税发票定义
     */
    @RequestMapping(method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Long create(@RequestBody UserVatInvoice userVatInvoice) {
        Response<Long> resp = userVatInvoiceWriteService.create(userVatInvoice, UserUtil.getCurrentUser());
        if (!resp.isSuccess()) {
            log.error("failed to create vat={}, user={}, {}", userVatInvoice, UserUtil.getCurrentUser(), resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    /**
     * 更新增值税发票定义
     * @param userVatInvoice    增值税发票定义
     */
    @RequestMapping(method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean update(@RequestBody UserVatInvoice userVatInvoice) {
        Response<Boolean> resp = userVatInvoiceWriteService.update(userVatInvoice, UserUtil.getCurrentUser());
        if (!resp.isSuccess()) {
            log.error("failed to update vat={}, user={}, {}", userVatInvoice, UserUtil.getCurrentUser(), resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    /**
     * 获取当前用户的增值税发票
     */
    @RequestMapping(method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public UserVatInvoice get() {
        Response<UserVatInvoice> resp = userVatInvoiceReadService.getByUser(UserUtil.getCurrentUser());
        if (!resp.isSuccess()) {
            log.error("failed to query vat, user={}, {}", UserUtil.getCurrentUser(), resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

}
