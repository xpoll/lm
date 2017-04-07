package cn.blmdz.rabbit.admin.business;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.user.model.Business;
import cn.blmdz.rabbit.user.service.BusinessReadService;
import cn.blmdz.rabbit.user.service.BusinessWriteService;
import cn.blmdz.wolf.user.model.User;
import lombok.extern.slf4j.Slf4j;

/**
 * 业务授权操作
 *
 * Author  : liushaofei
 * Date    : 5:54 PM 7/29/16
 */
@Slf4j
@RestController
@RequestMapping("/api/admin/business")
public class AdminBussinesses {
    private final BusinessReadService businessReadService;
    private final BusinessWriteService businessWriteService;

    @Autowired
    public AdminBussinesses(BusinessReadService businessReadService, BusinessWriteService businessWriteService){
        this.businessReadService = businessReadService;
        this.businessWriteService = businessWriteService;
    }


    @RequestMapping("selectById")
    public Object selectById(@RequestParam("id")Long id){
        Business business = new Business();
        business.setId(id);
        Response<Paging<Business>> resp = businessReadService.select(business,0,1);
        if (!resp.isSuccess()){
            throw new JsonResponseException(resp.getError());
        }
        return resp.getResult();
    }

    @RequestMapping("/selectByBusinessNameAndType")
    public Paging<Business> select(@RequestParam(value = "pageNo", required = false) Integer pageNo,
                         @RequestParam(value = "size", required = false) Integer size,
                         @RequestParam(value = "businessName", required = false)String businessName,
                         @RequestParam(value = "type", required = false)Integer type){
        if(pageNo == null){
            pageNo = 0;
        }
        if(size == null){
            size = 20;
        }
        if(businessName.equals("")){
            businessName = null;
        }
        Business business = new Business();
        business.setBusinessName(businessName);
        business.setType(type);
        Response<Paging<Business>> resp = businessReadService.select(business, pageNo, size);
        if (!resp.isSuccess()){
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    @RequestMapping("/insert")
    public Boolean insert(@RequestBody Business business){
        Business exit = new Business();
        exit.setType(business.getType());
        exit.setBusinessName(business.getBusinessName());
        Response<Paging<Business>> exitResp = businessReadService.select(exit, 0, 1);
        List<Business> list = exitResp.getResult().getData();
        if (!(list == null || list.size() <= 0)){
            throw new JsonResponseException(500, "同类型不可同名");
        }
        Response<Boolean> resp = businessWriteService.insert(business);
        if (!resp.isSuccess()){
            log.error("failed to insert article by business = {}, cause : ", business, resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    @RequestMapping("/delete")
    public Boolean delete(@RequestParam Business business){
        Response<Boolean> resp = businessWriteService.delete(business);
        if (!resp.isSuccess()){
            log.error("failed to delete article by business = {}, cause : ", business, resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    @RequestMapping("/deleteById")
    public Boolean deleteById(@RequestParam Long id){
        Business business = new Business();
        business.setId(id);
        Response<Boolean> resp = businessWriteService.delete(business);
        if (!resp.isSuccess()){
            log.error("faild to delete article by id = {}, cause : ", business.getId(),resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }


    @RequestMapping("/updateById")
    public Boolean updateById(@RequestBody Business business){
        Map<String, Object> map = new HashedMap();
        Business change = business;
        business = new Business();
        business.setId(change.getId());
        change.setId(null);
        map.put("business", business);
        map.put("change", change);
        Response<Boolean> resp = businessWriteService.update(map);
        if (!resp.isSuccess()) {
            log.error("failed to update article by business = {}, cause : ", business, resp.getError());
            throw new JsonResponseException(500, resp.getError());
        }
        return resp.getResult();
    }

    @RequestMapping("/user")
    public Object user(){
        User user = UserUtil.getCurrentUser();
        return user;
    }
}
