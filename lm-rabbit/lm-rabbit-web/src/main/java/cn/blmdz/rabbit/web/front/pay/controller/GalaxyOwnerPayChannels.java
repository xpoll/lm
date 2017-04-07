package cn.blmdz.rabbit.web.front.pay.controller;

import static cn.blmdz.home.common.util.Arguments.notNull;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.pay.enums.PayChannelEnum;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.pay.dto.OwnerPayChannelDto;
import cn.blmdz.wolf.pay.dto.PayChannelDto;
import cn.blmdz.wolf.pay.model.OwnerPayChannel;
import cn.blmdz.wolf.pay.service.PayReadService;
import cn.blmdz.wolf.pay.service.PayWriteService;
import cn.blmdz.wolf.web.pay.controller.OwnerPayChannels;
import lombok.extern.slf4j.Slf4j;

/**
 * 获取当前支付渠道列表control
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 15/4/1
 * Time: 上午10:43
 */
@Controller
@Slf4j
@RequestMapping("/api/owner")
public class GalaxyOwnerPayChannels extends OwnerPayChannels {


    private final OrderReadService orderReadService;
    private final ShopReadService shopReadService;

    @Autowired
    public GalaxyOwnerPayChannels(OrderReadService orderReadService, ShopReadService shopReadService, PayReadService payReadService, SiteService siteService, PayWriteService payWriteService) {
        super(payReadService, siteService, payWriteService);
        this.orderReadService = orderReadService;
        this.shopReadService = shopReadService;
    }

    /**
     * 获取当前支付渠道列表
     */
    @RequestMapping(value = "/pay_channel", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    @Override
    public OwnerPayChannel getOwnerPayChannel(String orderIds, Integer orderType, HttpServletRequest request) {

        Long shopOwnerId=null;

        List<Long> ids = Splitters.splitToLong(orderIds, Splitters.COMMA);
        //店铺
        if(ids.size()==1&&!orderType.equals(OrderType.MERGE_ORDER.value())){
            Long shopId = 0L;

            if(orderType.equals(OrderType.SHOP_ORDER.value())){

                Response<ShopOrder> shopOrderRes = orderReadService.findShopOrderById(ids.get(0));
                if(!shopOrderRes.isSuccess()){
                    throw new JsonResponseException(shopOrderRes.getError());
                }
                shopId = shopOrderRes.getResult().getShopId();
            }

            if(orderType.equals(OrderType.SKU_ORDER.value())){

                Response<SkuOrder> skuOrderRes = orderReadService.findSkuOrderById(ids.get(0));
                if(!skuOrderRes.isSuccess()){
                    throw new JsonResponseException(skuOrderRes.getError());
                }

                shopId = skuOrderRes.getResult().getShopId();

            }

            Response<Shop> shopRes = shopReadService.findById(shopId);
            if(!shopRes.isSuccess()){
                throw new JsonResponseException(shopRes.getError());
            }

            shopOwnerId=shopRes.getResult().getUserId();

        }
        return super.getOwnerPayChannel( shopOwnerId,request);
    }


    /**
     * 创建支付渠道拥有者信息
     */
    @RequestMapping(value = "/pay_channel", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean createOwnerPayChannel(@RequestBody OwnerPayChannel ownerPayChannel){

        Response<Boolean> response = payWriteService.createOwnerPayChannel(ownerPayChannel);
        if(!response.isSuccess()){
            throw new JsonResponseException(response.getError());
        }

        return response.getResult();

    }

    /**
     * 修改支付渠道拥有者信息
     */
    @RequestMapping(value = "/pay_channel", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public Boolean updateOwnerPayChannel(@RequestBody OwnerPayChannel ownerPayChannel){

        Response<Boolean> response = payWriteService.updateOwnerPayChannel(ownerPayChannel);
        if(!response.isSuccess()){
            throw new JsonResponseException(response.getError());
        }

        return response.getResult();

    }

    /**
     * 获取支付渠道拥有者信息
     */
    @RequestMapping(value = "/current_pay_channel", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public OwnerPayChannelDto getCurrentOwnerPayChannel(@RequestParam(value = "id") Long id ,@RequestParam(value = "type") Integer type){

        Response<OwnerPayChannel> opcRes = payReadService.findOwnerPayChannelByOwnerIdAndType(id,type);
        if(!opcRes.isSuccess()){
            throw new JsonResponseException(opcRes.getError());
        }
        OwnerPayChannel opc = opcRes.getResult();
        OwnerPayChannelDto opcd = new OwnerPayChannelDto();
        List<String> channels =null;
        if(notNull(opc)){
            channels =Splitters.COMMA.splitToList(opc.getChannel());
            opcd.setId(opc.getId());
        }

        List<PayChannelDto> pcds = Lists.newArrayList();
        for (PayChannelEnum pc : PayChannelEnum.values()){
            PayChannelDto pcd = new PayChannelDto();
            pcd.setName(pc.name);
            pcd.setDescription(pc.description);
            if(notNull(channels)&&channels.contains(pc.name)){
                pcd.setIsChecked(Boolean.TRUE);
            }
            pcds.add(pcd);
        }
        opcd.setPayChannels(pcds);

        return opcd;
    }


}
