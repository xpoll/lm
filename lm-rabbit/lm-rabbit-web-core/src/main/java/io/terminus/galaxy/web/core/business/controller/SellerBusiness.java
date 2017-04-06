package io.terminus.galaxy.web.core.business.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.api.client.repackaged.com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.enums.BusinessType;
import io.terminus.galaxy.user.model.Business;
import io.terminus.galaxy.user.model.Seller;
import io.terminus.galaxy.user.service.BusinessReadService;
import io.terminus.galaxy.user.service.SellerReadService;
import io.terminus.galaxy.user.service.SellerWriteService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Created by liushaofei on 16/8/3.
 */
@Slf4j
@RestController
@RequestMapping("/api/business")
public class SellerBusiness {

    private final SellerReadService sellerReadService;
    private final SellerWriteService sellerWriteService;
    private final BusinessReadService businessReadService;
    @Autowired
    public SellerBusiness(SellerReadService sellerReadService, SellerWriteService sellerWriteService, BusinessReadService businessReadService){
        this.sellerReadService = sellerReadService;
        this.sellerWriteService = sellerWriteService;
        this.businessReadService = businessReadService;
    }

    @RequestMapping("/getPermitBusiness")
    public Object getPermitBusiness(Long id){
        Seller seller = new Seller();
        Response<Seller> resp = sellerReadService.findByShopId(id);
        if(!resp.isSuccess()){
            throw new JsonResponseException(resp.getError());
        }
        seller = resp.getResult();
        Map<String, String> extra = seller.getExtra();
        List<Business> allBusinesses = null;

        ArrayList<Integer> adminBusinessIdsList = null;
        ArrayList<Long> adminBusinessIdsListLong = new ArrayList<>();
        try{
            ObjectMapper objectMapper = new ObjectMapper();
            String adminBusinessIds = extra.get("adminBusinessIds");
            if(adminBusinessIds != null && !"".equals(adminBusinessIds)){
                adminBusinessIdsList = objectMapper.readValue(adminBusinessIds, ArrayList.class);
            }
            if(adminBusinessIdsList != null){
                for (Integer obj: adminBusinessIdsList){
                    adminBusinessIdsListLong.add(Long.valueOf(obj.toString()));
                }
            }
            allBusinesses = businessReadService.select(new Business(), null, null).getResult().getData();
        }catch (IOException e){
            log.error("get permit business failed, error = {}", Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("500 json解析错误");
        }
        List<Map<String, Object>> data = new ArrayList<>();
        for (Business obj : allBusinesses){
            Map<String, Object> temp = new HashedMap();
            data.add(temp);
            temp.put("id", obj.getId());
            boolean flag = false;
            for(int i = 0, len = adminBusinessIdsListLong.size(); i < len; i++){
                if(adminBusinessIdsListLong.get(i).toString().equals(obj.getId().toString())){
                    flag = true;
                }
            }
            if (flag){
                temp.put("has", true);
            }
            else {
                temp.put("has", false);
            }
            temp.put("type", obj.getType());
            temp.put("businessName", obj.getBusinessName());
            temp.put("createdAt", obj.getCreatedAt());
            temp.put("updatedAt", obj.getUpdatedAt());
            temp.put("img", obj.getImg());
            temp.put("description", obj.getDescription());
            temp.put("businessLink", obj.getBusinessLink());
        }

        Map<String, Object> res = new HashedMap();
        BusinessType[] types = BusinessType.values();
        for (BusinessType obj : types){
            List<Map<String, Object>> list = new LinkedList<>();
            res.put(obj.getDescription(), list);
            for(Map<String, Object> busin : data){
                if(busin.get("type") != null && Integer.valueOf(obj.value()).toString().equals(busin.get("type").toString())){
                    list.add(busin);
                }

            }
        }

        return res;
    }
    @RequestMapping("/getSelectBusiness")
    public Object getSelectBusiness(Long id){
        Seller seller = new Seller();
        Response<Seller> resp = sellerReadService.findByShopId(id);
        if(!resp.isSuccess()){
            throw new JsonResponseException(resp.getError());
        }
        seller = resp.getResult();
        Map<String, String> extra = seller.getExtra();
        List<Business> adminBusinesses = null;
        List<Business> sellerBusinesses = null;
        List<Long> tags = new ArrayList<>();
        try{
            ObjectMapper objectMapper = new ObjectMapper();
            String adminBusinessIds = extra.get("adminBusinessIds");
            ArrayList<Integer> adminBusinessIdsList = new ArrayList<>();
            if(adminBusinessIds != null && !"".equals(adminBusinessIds)){
                 adminBusinessIdsList = objectMapper.readValue(adminBusinessIds, ArrayList.class);
            }
            String sellerBusinessIds = extra.get("sellerBusinessIds");
            ArrayList<Integer> sellerBusinessIdsList = new ArrayList<>();
            if(sellerBusinessIds != null && !"".equals(sellerBusinessIds)){
                sellerBusinessIdsList = objectMapper.readValue(sellerBusinessIds,ArrayList.class);
            }
            ArrayList<Long> adminBusinessIdsListLong = new ArrayList<>();
            for (Integer obj: adminBusinessIdsList){
                adminBusinessIdsListLong.add(Long.valueOf(obj.toString()));
            }
            if(adminBusinessIdsListLong.size() == 0){
                adminBusinessIdsListLong = null;
            }
            adminBusinesses = businessReadService.findByIds(adminBusinessIdsListLong);
            for(int i =0, len = sellerBusinessIdsList.size(); i < len; i++){
                if(adminBusinessIdsList.contains(sellerBusinessIdsList.get(i))){
                    tags.add(new Long(sellerBusinessIdsList.get(i).toString()));
                }
            }
        }catch (IOException e){
            log.error("get permit business failed, error = {}", Throwables.getStackTraceAsString(e));
            throw new JsonResponseException("500 json解析错误");
        }
        List<Map<String, Object>> data = new ArrayList<>();
        for (Business obj : adminBusinesses){
            Map<String, Object> temp = new HashedMap();
            data.add(temp);
            temp.put("id", obj.getId());
            boolean flag = false;
            for(int i = 0, len = tags.size(); i < len; i++){
                if(tags.get(i).toString().equals(obj.getId().toString())){
                    flag = true;
                }
            }
            if (flag){
                temp.put("has", true);
            }
            else {
                temp.put("has", false);
            }
            temp.put("type", obj.getType());
            temp.put("businessName", obj.getBusinessName());
            temp.put("createdAt", obj.getCreatedAt());
            temp.put("updatedAt", obj.getUpdatedAt());
            temp.put("img", obj.getImg());
            temp.put("description", obj.getDescription());
            temp.put("businessLink", obj.getBusinessLink());
        }

        Map<String, Object> res = new HashedMap();
        BusinessType[] types = BusinessType.values();
        for (BusinessType obj : types){
            List<Map<String, Object>> list = new LinkedList<>();
            res.put(obj.getDescription(), list);
            for(Map<String, Object> busin : data){
                if(busin.get("type") != null && Integer.valueOf(obj.value()).toString().equals(busin.get("type").toString())){
                    list.add(busin);
                }

            }
        }

        return res;
    }

    /**
     *
     * @param adminBusinessIds
     * @param id
     * @param sellerBusinessIds
     * @return
     */
    @RequestMapping(value = "/updateBusinessIds",method = RequestMethod.POST)
    public Object updateBusinessIds(@RequestParam(value = "adminBusinessIds", required = false) String adminBusinessIds,
                                    @RequestParam("id") Long id,
                                    @RequestParam(value = "sellerBusinessIds", required = false)String sellerBusinessIds){
        Seller seller = new Seller();
        seller.setId(id);
        Response<Seller> respSelect = sellerReadService.findByShopId(id);
        if (!respSelect.isSuccess()){
            throw new JsonResponseException("该商家不存在");
        }
        seller = respSelect.getResult();
        Map<String, String> extra = seller.getExtra();
        ObjectMapper objectMapper = new ObjectMapper();
        try{
            if (adminBusinessIds != null && !"".equals(adminBusinessIds)){
                String[] adminIds = adminBusinessIds.split(",");
                extra.put("adminBusinessIds", changeListJson(adminIds));
            }
            if (sellerBusinessIds != null && !"".equals(sellerBusinessIds)){
                String[] sellerIds = sellerBusinessIds.split(",");
                extra.put("sellerBusinessIds", changeListJson(sellerIds));
            }
        }catch (JsonProcessingException e){
            throw new JsonResponseException("参数格式错误");
        }
        seller.setExtra(extra);
        Response<Boolean> resp = sellerWriteService.updateSeller(seller);
        if (!resp.isSuccess()){
            throw  new JsonResponseException(resp.getError());
        }
        return resp.getResult();
    }
    public String changeListJson(String[] array) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        List<Long> list = new ArrayList<>();
        for (String obj : array){
            Business business = new Business();
            business.setId(Long.valueOf(obj));
            business = businessReadService.findById(Long.valueOf(obj));
            if (business == null){
                continue;
            }
            list.add(business.getId());
        }
        return mapper.writeValueAsString(list);
    }

}
