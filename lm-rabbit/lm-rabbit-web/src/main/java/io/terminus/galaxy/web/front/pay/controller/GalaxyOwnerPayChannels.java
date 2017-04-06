package io.terminus.galaxy.web.front.pay.controller;

import com.google.common.collect.Lists;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.pay.enums.PayChannelEnum;
import io.terminus.pampas.design.service.SiteService;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.pay.dto.OwnerPayChannelDto;
import io.terminus.parana.pay.dto.PayChannelDto;
import io.terminus.parana.pay.model.OwnerPayChannel;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import io.terminus.parana.web.pay.controller.OwnerPayChannels;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

import static io.terminus.common.utils.Arguments.notNull;

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
