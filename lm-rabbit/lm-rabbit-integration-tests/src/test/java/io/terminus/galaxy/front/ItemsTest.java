/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.front;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import configuration.FrontWebConfiguration;
import io.terminus.galaxy.BaseWebTest;
import io.terminus.parana.attribute.dto.GroupedOtherAttribute;
import io.terminus.parana.attribute.dto.GroupedSkuAttribute;
import io.terminus.parana.attribute.dto.OtherAttribute;
import io.terminus.parana.attribute.dto.SkuAttribute;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.item.dto.ImageInfo;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemDetail;
import io.terminus.parana.item.model.Sku;
import org.junit.After;
import org.junit.Test;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.boot.test.TestRestTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Random;

import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-30
 */
@SpringApplicationConfiguration({FrontWebConfiguration.class})
public class ItemsTest extends BaseWebTest {

    private RestTemplate restTemplate = new TestRestTemplate();

    @Test
    public void testValidItem() throws Exception {
        FullItem fullItem = makeFullItem();

        Long id = restTemplate.postForObject("http://localhost:{port}/api/seller/items", fullItem,
                Long.class, ImmutableMap.of("port", port));
        assertThat(id, notNullValue());
    }

    private FullItem makeFullItem() throws Exception {
        FullItem fullItem = new FullItem();

        Item item = new Item();
        item.setCategoryId(1L);
        item.setName("testItem1");
        item.setBrandId(1L);
        item.setBrandName("nike");
        item.setExtra(ImmutableMap.of("unit","双"));
        item.setMainImage("http://taobao.com");

        fullItem.setItem(item);

        ItemDetail itemDetail = new ItemDetail();
        itemDetail.setDetail("i am detail");
        itemDetail.setImages(ImmutableList.of(new ImageInfo("xx", "http://xx0oo.com")));
        itemDetail.setPacking(ImmutableMap.of("包邮","true"));

        fullItem.setItemDetail(itemDetail);

        SkuAttribute color_red = new SkuAttribute();
        color_red.setAttrKey("颜色");
        color_red.setAttrVal("红色");

        SkuAttribute color_black = new SkuAttribute();
        color_black.setAttrKey("颜色");
        color_black.setAttrVal("黑色");

        SkuAttribute size_110 = new SkuAttribute();
        size_110.setAttrKey("尺码");
        size_110.setAttrVal("110");
        size_110.setUnit("cm");

        SkuAttribute size_120 = new SkuAttribute();
        size_120.setAttrKey("尺码");
        size_120.setAttrVal("120");
        size_120.setUnit("cm");


        GroupedSkuAttribute colorGroup = new GroupedSkuAttribute("颜色",ImmutableList.of(color_black,color_red));
        GroupedSkuAttribute sizeGroup = new GroupedSkuAttribute("尺码",ImmutableList.of(size_110,size_120));

        fullItem.setGroupedSkuAttributes(ImmutableList.of(colorGroup,sizeGroup));

        OtherAttribute address = new OtherAttribute();
        address.setAttrKey("产地");
        address.setAttrVal("杭州");
        GroupedOtherAttribute groupedOtherAttribute = new GroupedOtherAttribute("其他参数",ImmutableList.of(address));

        fullItem.setGroupedOtherAttributes(ImmutableList.of(groupedOtherAttribute));

        fullItem.setSkus(ImmutableList.of(makeSku(1)));
        return fullItem;
    }

    @Test
    public void testOnlyOneSkuAttribute() throws Exception {  //允许在两个sku候选中只选择一个作为sku属性

        FullItem onlyOneSkuAttributeItem = makeFullItem();

        //删掉一个sku属性
        SkuAttribute size_120 = new SkuAttribute();
        size_120.setAttrKey("尺码");
        size_120.setAttrVal("120");
        size_120.setUnit("cm");


        SkuAttribute color_red = new SkuAttribute();
        color_red.setAttrKey("颜色");
        color_red.setAttrVal("红色");

        OtherAttribute other_size_120 = new OtherAttribute();
        other_size_120.setAttrKey("尺码");
        other_size_120.setAttrVal("120");
        other_size_120.setUnit("cm");

        OtherAttribute address = new OtherAttribute();
        address.setAttrKey("产地");
        address.setAttrVal("杭州");


        GroupedOtherAttribute groupedOtherAttribute = new GroupedOtherAttribute("其他参数", Lists.newArrayList(other_size_120,address));

        onlyOneSkuAttributeItem.setGroupedOtherAttributes(ImmutableList.of(groupedOtherAttribute));

        final List<GroupedSkuAttribute> groupedSkuAttributes = ImmutableList.of(new GroupedSkuAttribute("颜色", ImmutableList.of(color_red)));
        onlyOneSkuAttributeItem.setGroupedSkuAttributes(groupedSkuAttributes);

        Sku sku = onlyOneSkuAttributeItem.getSkus().get(0);
        sku.setAttrs(ImmutableList.of(color_red));


        ResponseEntity<Long> result = restTemplate.postForEntity("http://localhost:{port}/api/seller/items", onlyOneSkuAttributeItem,
                Long.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), is(HttpStatus.OK));

    }

    @Test
    public void testBadSkuAttribute() throws Exception {   //sku属性不合法
        FullItem badSkuAttributeItem = makeFullItem();

        //一个不合法的sku属性
        SkuAttribute size_120 = new SkuAttribute();
        size_120.setAttrKey("foo");
        size_120.setAttrVal("120");
        size_120.setUnit("cm");

        final List<GroupedSkuAttribute> groupedSkuAttributes = ImmutableList.of(new GroupedSkuAttribute("foo", ImmutableList.of(size_120)));
        badSkuAttributeItem.setGroupedSkuAttributes(groupedSkuAttributes);

        ResponseEntity<String> result = restTemplate.postForEntity("http://localhost:{port}/api/seller/items", badSkuAttributeItem,
                String.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), not(is(HttpStatus.OK)));
    }

    @Test
    public void testMissRequiredAttribute() throws Exception {   //没有选择必须的其他属性
        FullItem missRequiredAttributeItem = makeFullItem();

        OtherAttribute address = new OtherAttribute();
        address.setAttrKey("等级");
        address.setAttrVal("bar");

        final List<GroupedOtherAttribute> groupedOtherAttributes = ImmutableList.of(new GroupedOtherAttribute("其他参数", ImmutableList.of(address)));
        missRequiredAttributeItem.setGroupedOtherAttributes(groupedOtherAttributes);

        ResponseEntity<String> result = restTemplate.postForEntity("http://localhost:{port}/api/seller/items", missRequiredAttributeItem,
                String.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), not(is(HttpStatus.OK)));
    }

    @Test
    public void testBadOtherAttribute() throws Exception {   //其他属性值不合法
        FullItem badOtherAttributeItem = makeFullItem();

        OtherAttribute grade = new OtherAttribute();
        grade.setAttrKey("等级");
        grade.setAttrVal("bar");

        final GroupedOtherAttribute groupedGrade = new GroupedOtherAttribute("其他参数", ImmutableList.of(grade));

        OtherAttribute address = new OtherAttribute();
        address.setAttrKey("产地");
        address.setAttrVal("杭州");
        GroupedOtherAttribute groupedOtherAttribute = new GroupedOtherAttribute("其他参数",ImmutableList.of(address));

        final List<GroupedOtherAttribute> groupedOtherAttributes = ImmutableList.of(groupedOtherAttribute,groupedGrade);
        badOtherAttributeItem.setGroupedOtherAttributes(groupedOtherAttributes);

        ResponseEntity<String> result = restTemplate.postForEntity("http://localhost:{port}/api/seller/items", badOtherAttributeItem,
                String.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), not(is(HttpStatus.OK)));
    }

    @Test
    public void testBadValueTypeItem() throws Exception {
        FullItem badValueTypeItem = makeFullItem();

        OtherAttribute weight = new OtherAttribute();
        weight.setAttrKey("重量");
        weight.setAttrVal("foobar");
        GroupedOtherAttribute groupedWeight = new GroupedOtherAttribute("其他参数",ImmutableList.of(weight));

        OtherAttribute address = new OtherAttribute();
        address.setAttrKey("产地");
        address.setAttrVal("杭州");
        GroupedOtherAttribute groupedOtherAttribute = new GroupedOtherAttribute("其他参数",ImmutableList.of(address));

        final List<GroupedOtherAttribute> groupedOtherAttributes = ImmutableList.of(groupedOtherAttribute,groupedWeight);
        badValueTypeItem.setGroupedOtherAttributes(groupedOtherAttributes);

        ResponseEntity<String> result = restTemplate.postForEntity("http://localhost:{port}/api/seller/items", badValueTypeItem,
                String.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), not(is(HttpStatus.OK)));
    }


    @Test
    public void testNoCategoryAttributeButItemHasSkuAttributesTest() throws Exception {
        FullItem fullItem = makeFullItem();

        //category(id=3) 没有任何属性(包括销售属性), 那么该类目下的商品也不允许定义销售属性,(但允许有sku)
        fullItem.getItem().setCategoryId(3L);

        ResponseEntity<String> result =  restTemplate.postForEntity("http://localhost:{port}/api/seller/items", fullItem,
                String.class, ImmutableMap.of("port", port));
        assertThat(result.getStatusCode(), not(is(HttpStatus.OK)));

    }

    @Test
    public void testNoCategoryAttributeTest() throws Exception {
        FullItem fullItem = makeFullItem();

        //category(id=3) 没有任何属性(包括销售属性), 那么该类目下的商品也不允许定义销售属性,(但允许有sku)
        fullItem.getItem().setCategoryId(3L);

        fullItem.setGroupedSkuAttributes(null);

        for (Sku sku : fullItem.getSkus()) {
            sku.setAttrs(null);
        }

        Long id =  restTemplate.postForObject("http://localhost:{port}/api/seller/items", fullItem,
                Long.class, ImmutableMap.of("port", port));
        assertThat(id, notNullValue());

    }

    @After
    public void tearDown() throws Exception {
        /*restTemplate.delete("http://localhost:{port}/api/frontCategories/{id}",
                ImmutableMap.of("port", port, "id", frontCategory.getId()));*/
    }

    private Sku makeSku(long i) throws Exception{
        Sku sku = new Sku();
        Random r = new Random();
        sku.setSkuCode("skuCode"+i);
        sku.setSpecification("ZUC-CW000-RED");
        sku.setStatus(1);
        sku.setOuterSkuId("outsku"+i);
        sku.setOuterShopId("outshop"+i);
        sku.setImage("image"+i);
        sku.setThumbnail("thumbnail"+i);
        sku.setName("sku"+i);
        sku.setPrice(6000);
        SkuAttribute color_red = new SkuAttribute();
        color_red.setAttrKey("颜色");
        color_red.setAttrVal("红色");

        SkuAttribute size_120 = new SkuAttribute();
        size_120.setAttrKey("尺码");
        size_120.setAttrVal("120");
        size_120.setUnit("cm");

        sku.setAttrs(ImmutableList.of(color_red,size_120));
        sku.setStockQuantity(r.nextInt());
        sku.setStockType(0);
        sku.setExtraPrice(ImmutableMap.of("originPrice",3, "platformPrice", 4));
        return sku;
    }
}
