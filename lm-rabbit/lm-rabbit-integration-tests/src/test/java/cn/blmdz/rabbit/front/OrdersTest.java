package cn.blmdz.rabbit.front;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.TestRestTemplate;
import org.springframework.web.client.RestTemplate;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.rabbit.BaseWebTest;
import cn.blmdz.rabbit.order.dto.FatOrder;
import cn.blmdz.rabbit.order.dto.FatSku;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.user.address.model.ReceiveAddress;
import configuration.FrontWebConfiguration;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-04-01 5:16 PM  <br>
 * Author: xiao
 */
@SpringApplicationConfiguration({FrontWebConfiguration.class})
public class OrdersTest extends BaseWebTest {

    private RestTemplate restTemplate = new TestRestTemplate();

    @Autowired
    private OrderReadService orderReadService;

    @Test
    public void testCreateOrder() {
        List<FatOrder> fatOrderList = makeFatOrders(1L);
        ReceiveAddress receiveAddress = makeReceiveAddress(1L);
        Map<String, Object> requestParams = new HashMap<>();
        requestParams.put("tradeInfo", receiveAddress);
        requestParams.put("data", JsonMapper.nonDefaultMapper().toJson(fatOrderList));

        List result = postFormForObject("/api/order/create", requestParams,
                List.class);
        assertNotNull(result);
    }

    private List<FatOrder> makeFatOrders(Long i) {
        FatOrder fatOrder = new FatOrder();
        fatOrder.setShopId(i);
        fatOrder.setFlowId(i);
        fatOrder.setBuyerNotes("buyerNote" + i);
        fatOrder.setPayType(1);
        List<FatSku> fatSkus = new ArrayList<>();
        FatSku fatSku = new FatSku();
        fatSku.setSkuId(i);
        fatSku.setQuantity(1);
        fatSkus.add(fatSku);
        fatOrder.setFatSkus(fatSkus);
        return Lists.newArrayList(fatOrder);
    }

    private ReceiveAddress makeReceiveAddress(Long i) {
        ReceiveAddress receiveAddress = new ReceiveAddress();
        receiveAddress.setId(i);
        receiveAddress.setUserId(i);
        receiveAddress.setReceiveUserName("name" + i);
        receiveAddress.setPhone("18987899878");
        receiveAddress.setIsDefault(Boolean.TRUE);
        receiveAddress.setStatus(1);
        receiveAddress.setProvinceId(1);
        receiveAddress.setProvince("province" + i);
        receiveAddress.setCityId(1);
        receiveAddress.setCity("city" + i);
        receiveAddress.setRegionId(1);
        receiveAddress.setRegion("region" + i);
        return receiveAddress;
    }

    @Test
    public void testOrderBuyerCancel() {
        Map<String, Object> requestParams = new HashMap<>();
        requestParams.put("actionInstanceId", 4);
        Map<String, Object> updateParams = new HashMap<>();
        updateParams.put("orderId", 71L);
        requestParams.put("updateContext", JsonMapper.nonDefaultMapper().toJson(updateParams));
        putFormForObject("/api/order/update", requestParams, Map.class);

        Response<ShopOrder> shopOrderResponse = orderReadService.findShopOrderById(71L);
        assertThat(shopOrderResponse.getResult().getNodeInstanceId(), is(5L));
    }
}